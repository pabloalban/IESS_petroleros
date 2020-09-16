# Se proyectan los salarios futuros de los cotizantes a cesantía y desempleo, tomando en cuenta:
# 1) los salarios promedio de sus cotizantes en el 2018;
# 2) aplicando la distribución del número de cotizaciones por edad de los cotizantes al SGO (p de cot_prop)
# 2) aplicando los incrementos por años de cotización de los del SGO (sal_inc_model)
# 3) la ER se calcula de los cotizantes a cesantía desde el 2014 a 2018 (5 años)
message(paste(rep("-", 100), collapse = ""))

message("\tGenerando proyección de salarios y aportes")

load(paste0(parametros$RData, "IESS_salarios_pensiones_iniciales_v3.RData"))
load(paste0(parametros$RData, "IESS_probabilidades_transicion_edad_tiempo_servicio.RData"))
load(paste0(parametros$RData, "IESS_macro_estudio.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_salarios.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData"))

# Horizonte de proyección
t_max <- parametros$horizonte
i_sbu <- Hipotesis[which(Hipotesis$Hipotesis == "Crecimiento_SBU"), ]$Proyeccion
i_r <- Hipotesis[which(Hipotesis$Hipotesis == "Crecimiento_Salarial"), ]$Proyeccion

# Proyección de salario básico unificado (SBU) -----------------------------------------------------
sbu_proy <- data.table(t = 0:t_max)
sbu_proy[, i_sbu := i_sbu]
sbu_proy[, sbu := 386 * (1 + i_sbu)^t]
setorder(sbu_proy, t)
sbu_proy <- sbu_proy[, list(t, i_sbu, sbu)]

# Calculando exposicion ----------------------------------------------------------------------------
# Se calcula para el período 2014 a 2018 (5 años)
ER <- cotizantes_ces_des %>%
  group_by(edad, genero) %>%
  filter(aniper > 2013) %>%
  mutate(ER_sal = mean(cotizantes, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(edad, genero, .keep_all = TRUE) %>%
  select(edad, genero, ER_sal)

sal_afi <- salarios_cotizantes %>%
  filter(anio == 2018) %>%
  select(sexo, edad, sal) %>%
  left_join(ER, by = c("edad" = "edad", "sexo" = "genero")) %>%
  ungroup()

# Proyección salarios ------------------------------------------------------------------------------
message("\tGenerando proyección de salarios")
sal_afi <- as.data.table(sal_afi)
sal_afi[, log_sal := log(sal)]

sal_int_f <- data.table(edad = 15:105)

# Suavisando los salarios---------------------------------------------------------------------------
sal_smooth_f <- lm(log_sal ~ bs(edad, df = 7, degree = 3),
  weights = ER_sal,
  data = sal_afi[sexo == "F" & edad >= 18 & edad <= 105]
)

sal_int_f[, log_sal := predict(object = sal_smooth_f, newdata = sal_int_f)]
sal_int_f[, sal := exp(log_sal)]
sal_int_f[sal <= 386, sal := 386]
sal_int_f[, sexo := "F"]

sal_int_m <- data.table(edad = 15:105)
sal_smooth_m <- lm(log_sal ~ bs(edad, df = 7, degree = 3),
  weights = ER_sal,
  data = sal_afi[sexo == "M" & edad >= 18 & edad <= 105]
)

sal_int_m[, log_sal := predict(object = sal_smooth_m, newdata = sal_int_m)]
sal_int_m[, sal := exp(log_sal)]
sal_int_m[sal <= 386, sal := 386]
sal_int_m[, sexo := "M"]

sal_int <- rbind(sal_int_f, sal_int_m)

# aux_f <- sal_int[ sexo == 'F' ]
# aux_ini_f <- sal_afi[ sexo == 'F' ]
# plot( aux_f$x, aux_f$sal, type = 'l', col = parametros$iess_blue, ylim = c( 0, 1500 ) )
# points( aux_ini_f$x, aux_ini_f$sal, type = 'p', col = parametros$iess_green, cex = 0.7, pch = 16 )

# aux_m <- sal_int[ sexo == 'M' ]
# aux_ini_m <- sal_afi[ sexo == 'M' ]
# plot( aux_m$x, aux_m$sal, type = 'l', col = parametros$iess_blue, ylim = c( 0, 1500 ) )
# points( aux_ini_m$x, aux_ini_m$sal, type = 'p', col = parametros$iess_green, cex = 0.7, pch = 16 )

sal_proy <- data.table(expand.grid(t = 0:t_max, sexo = c("F", "M"), edad = 15:105))
sal_proy <- merge(sal_proy, sal_inc_model, by.x = "edad", by.y = "x", all.x = TRUE, allow.cartesian = TRUE)
sal_proy[is.na(sal_inc), sal_inc := 0]
sal_proy <- merge(sal_proy, cot_prop[, list(edad = x, s, p2 = p)], all.x = TRUE, by = c("edad", "s"))
sal_proy[is.na(p2), p2 := 0]
sal_proy <- merge(sal_proy,
  sal_int[, list(sexo, edad, sal)],
  by = c("sexo", "edad"), all.x = TRUE
)
sal_proy[, i_r := i_r]
sal_proy[, sal_mes := sal * (1 + i_r)^t]
sal_proy[, sal_mes := (1 + sal_inc) * sal_mes]

sal_proy <- merge(sal_proy, sbu_proy, by = "t", all.x = TRUE, allow.cartesian = TRUE)
sal_proy[sal_mes <= sbu, sal_mes := sbu]

sal_proy[, sal := 12 * sal_mes]
sal_proy[, sal_mes := sal_mes * p2]
sal_proy[, sal := sal * p2]

sal_proy <- sal_proy[, list(
  sal_mes = sum(sal_mes),
  sal = sum(sal)
), by = list(t, sexo, edad)]
setorder(sal_proy, t, sexo, edad)
sal_proy <- sal_proy[, list(t, sexo, x = edad, i_r = i_r, sal_mes, sal)]

save(sal_int, sal_proy, sbu_proy,
  file = paste0(parametros$RData_seg, "IESS_CES_DES_proyeccion_salarios", ".RData")
)

message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros", "Hipotesis"))])
gc()
