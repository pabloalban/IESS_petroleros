message(paste(rep("-", 100), collapse = ""))

# Cargando información -----------------------------------------------------------------------------
message("\tCargando datos")
load(paste0(parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData"))
load(paste0(parametros$RData_seg, "IESS_DES_tasa_uso_edad_sexo_int.RData"))
load(paste0(parametros$RData, "IESS_proyeccion_poblacion.RData"))

message("\tProyentando de población de beneficiarios")

# Calculando los cot y bene de desempleo con pagos indebidos ---------------------------------------
densidad_cotizacion_int <- select(densidad_cotizacion_int, -den_cot)
tasa_siniestralidad_pagos_ind <- select(tasa_siniestralidad_pagos_ind, -tasa_uso) %>%
  reshape2::dcast(.,
    edad + sd_genero ~ sd_numero_pagos,
    value.var = "tasa_uso_int"
  )
colnames(tasa_siniestralidad_pagos_ind) <- c("x", "sexo", "p1", "p2", "p3", "p4", "p5")

pob_proy_edad_sexo_pagos_ind <- left_join(as.data.frame(pob_proy),
  densidad_cotizacion_int,
  by = c("x" = "edad", "sexo" = "genero")
) %>%
  left_join(tasa_siniestralidad_pagos_ind, by = c("x", "sexo")) %>%
  replace(., is.na(.), 0) %>%
  mutate(
    l2_cot_sgo = l2_cot,
    l2_cot = l2_cot * den_cot_int,
    lp1 = l2_ces * p1,
    lp2 = l2_ces * p2,
    lp3 = l2_ces * p3,
    lp4 = l2_ces * p4,
    lp5 = l2_ces * p5
  ) %>%
  select(t, sexo, x, l2_cot_sgo, l2_cot, l2_ces, lp1, lp2, lp3, lp4, lp5) %>%
  ungroup()

pob_proy_pagos_ind <- pob_proy_edad_sexo_pagos_ind %>%
  group_by(t, sexo) %>%
  mutate(
    l2_cot_sgo = sum(l2_cot_sgo, na.rm = TRUE),
    l2_cot = sum(l2_cot, na.rm = TRUE),
    l2_ces = sum(l2_ces, na.rm = TRUE),
    lp1 = sum(lp1, na.rm = TRUE),
    lp2 = sum(lp2, na.rm = TRUE),
    lp3 = sum(lp3, na.rm = TRUE),
    lp4 = sum(lp4, na.rm = TRUE),
    lp5 = sum(lp5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(t, sexo, .keep_all = TRUE) %>%
  select(-x)

# Calculando los cotizantes y beneficiarios del seguro de desempleo --------------------------------
# densidad_cotizacion_int <- select(densidad_cotizacion_int,-den_cot)
tasa_siniestralidad <- select(tasa_siniestralidad, -tasa_uso) %>%
  reshape2::dcast(.,
    edad + sd_genero ~ sd_numero_pagos,
    value.var = "tasa_uso_int"
  )
colnames(tasa_siniestralidad) <- c("x", "sexo", "p1", "p2", "p3", "p4", "p5")

pob_proy_edad_sexo <- left_join(as.data.frame(pob_proy),
  densidad_cotizacion_int,
  by = c("x" = "edad", "sexo" = "genero")
) %>%
  left_join(tasa_siniestralidad, by = c("x", "sexo")) %>%
  replace(., is.na(.), 0) %>%
  mutate(
    l2_cot_sgo = l2_cot,
    l2_cot = l2_cot * den_cot_int,
    lp1 = l2_ces * p1,
    lp2 = l2_ces * p2,
    lp3 = l2_ces * p3,
    lp4 = l2_ces * p4,
    lp5 = l2_ces * p5
  ) %>%
  select(t, sexo, x, l2_cot_sgo, l2_cot, l2_ces, lp1, lp2, lp3, lp4, lp5) %>%
  ungroup()

pob_proy <- pob_proy_edad_sexo %>%
  group_by(t, sexo) %>%
  mutate(
    l2_cot_sgo = sum(l2_cot_sgo, na.rm = TRUE),
    l2_cot = sum(l2_cot, na.rm = TRUE),
    l2_ces = sum(l2_ces, na.rm = TRUE),
    lp1 = sum(lp1, na.rm = TRUE),
    lp2 = sum(lp2, na.rm = TRUE),
    lp3 = sum(lp3, na.rm = TRUE),
    lp4 = sum(lp4, na.rm = TRUE),
    lp5 = sum(lp5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(t, sexo, .keep_all = TRUE) %>%
  select(-x)

# Guardando la proyección de la población cotizante y beneficiaría de desempleo --------------------
message("\tGuardando proyección de población")
save(lf, lm, pob_proy, pob_proy_edad_sexo, pob_proy_edad_sexo_pagos_ind, pob_proy_pagos_ind,
  file = paste0(parametros$RData_seg, "IESS_DES_proyeccion_poblacion.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros", "Hipotesis"))])
gc()
