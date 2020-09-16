message(paste(rep("-", 100), collapse = ""))

# Cargando información -----------------------------------------------------------------------------
message("\tCargando datos")
load(paste0(parametros$RData_seg, "IESS_DES_pagos_edad_sex.RData"))
load(paste0(parametros$RData, "IESS_macro_estudio.RData"))

# Preparando datos para la regresión ---------------------------------------------------------------
SBU <- SBU_Anual %>%
  filter(Item == "Observado") %>%
  select(Fecha, SBU)

siniestros_desempleo <- siniestros_desempleo %>%
  filter(sd_tipo_pago == "F", anio_pago < "2019") %>%
  select(
    anio_pago,
    sd_genero, edad,
    sd_numero_pagos,
    sd_valor_pagado,
    cumple_requisitos
  ) %>%
  left_join(SBU, by = c("anio_pago" = "Fecha")) %>%
  mutate(SBU_70 = 0.7 * SBU)

# Regresión para encontrar la función de pago del beneficio ----------------------------------------
message("\tEstimando alpha del modelo de beneficios")
# Hombres ------------------------------------------------------------------------------------------
# Pago1 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "M",
    sd_numero_pagos == 1
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p1_m <- mod
# summary(mod)
alpha_m <- c(mod$coefficients)

# Pago2 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "M",
    sd_numero_pagos == 2
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p2_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Pago3 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "M",
    sd_numero_pagos == 3
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p3_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Pago4 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "M",
    sd_numero_pagos == 4
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p4_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Pago5 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "M",
    sd_numero_pagos == 5
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p5_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Mujeres ------------------------------------------------------------------------------------------
# Pago1 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "F",
    sd_numero_pagos == 1
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p1_f <- mod
# summary(mod)
alpha_f <- c(mod$coefficients)

# Pago2 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "F",
    sd_numero_pagos == 2
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p2_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Pago3 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "F",
    sd_numero_pagos == 3
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p3_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Pago4 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "F",
    sd_numero_pagos == 4
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p4_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Pago5 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    cumple_requisitos == "Si",
    sd_genero == "F",
    sd_numero_pagos == 5
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_p5_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Crear un data frame de los coeficientes de la regresión-------------------------------------------
mod_benficio_des <- rbind(
  data.frame(pago = c("alp1", "alp2", "alp3", "alp4", "alp5"), sexo = "F", alpha = alpha_f),
  data.frame(pago = c("alp1", "alp2", "alp3", "alp4", "alp5"), sexo = "M", alpha = alpha_m)
)

# Regresión para encontrar la función de pago del beneficio con pagos indebidos---------------------
# Hombres ------------------------------------------------------------------------------------------
# Pago1 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "M",
    sd_numero_pagos == 1
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p1_m <- mod
# summary(mod)
alpha_m <- c(mod$coefficients)

# Pago2 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "M",
    sd_numero_pagos == 2
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p2_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Pago3 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "M",
    sd_numero_pagos == 3
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p3_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Pago4 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "M",
    sd_numero_pagos == 4
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p4_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Pago5----------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "M",
    sd_numero_pagos == 5
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p5_m <- mod
# summary(mod)
alpha_m <- c(alpha_m, mod$coefficients)

# Mujeres ------------------------------------------------------------------------------------------
# Pago1 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "F",
    sd_numero_pagos == 1
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p1_f <- mod
# summary(mod)
alpha_f <- c(mod$coefficients)

# Pago2 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "F",
    sd_numero_pagos == 2
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p2_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Pago3 --------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "F",
    sd_numero_pagos == 3
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p3_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Pago4---------------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "F",
    sd_numero_pagos == 4
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p4_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)

# Pago5----------------------------------------------------------------------------------------
aux <- siniestros_desempleo %>%
  filter(
    sd_genero == "F",
    sd_numero_pagos == 5
  )

mod <- lm(sd_valor_pagado ~ (SBU_70) + 0, data = aux)
mod_ind_p5_f <- mod
# summary(mod)
alpha_f <- c(alpha_f, mod$coefficients)


# Crear un data frame de los coeficientes de la regresión ------------------------------------------
mod_benficio_des_pagos_ind <- rbind(
  data.frame(pago = c("alp1", "alp2", "alp3", "alp4", "alp5"), sexo = "F", alpha = alpha_f),
  data.frame(pago = c("alp1", "alp2", "alp3", "alp4", "alp5"), sexo = "M", alpha = alpha_m)
)

# Guardando Rdata con los parámetros alpha ---------------------------------------------------------
message("\tGuardando modelo de estimación de beneficios")
save(mod_benficio_des, mod_benficio_des_pagos_ind,
  file = paste0(parametros$RData_seg, "IESS_DES_alpha_modelo_beneficios.RData")
)
# Guardando Rdata con los modelos de beneficios ----------------------------------------------------
save(mod_p1_m, mod_p2_m, mod_p3_m, mod_p4_m, mod_p5_m,
  mod_p1_f, mod_p2_f, mod_p3_f, mod_p4_f, mod_p5_f,
  mod_ind_p1_m, mod_ind_p2_m, mod_ind_p3_m, mod_ind_p4_m, mod_ind_p5_m,
  mod_ind_p1_f, mod_ind_p2_f, mod_ind_p3_f, mod_ind_p4_f, mod_ind_p5_f,
  file = paste0(parametros$RData_seg, "IESS_DES_modelo_beneficios.RData")
)
# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros", "Hipotesis"))])
gc()