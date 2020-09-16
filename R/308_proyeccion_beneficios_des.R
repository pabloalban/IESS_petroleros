message(paste(rep("-", 100), collapse = ""))

# Cargando información -----------------------------------------------------------------------------
message("\tEstimación de parámetros alpha de los beneficios")
load(paste0(parametros$RData_seg, "IESS_DES_alpha_modelo_beneficios.RData"))
load(paste0(parametros$RData_seg, "IESS_DES_proyeccion_poblacion.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_proyeccion_salarios.RData"))
# load( paste0( parametros$RData,'IESS_macro_estudio.RData') )

# Calculado beneficios de desempleo ----------------------------------------------------------------
# Preparando datos ---------------------------------------------------------------------------------
mod_benficio_des <- dcast(mod_benficio_des, sexo ~ pago, value.var = "alpha")

pob_proy_edad_sexo <- left_join(pob_proy_edad_sexo, sbu_proy, by = c("t")) %>%
  left_join(mod_benficio_des, by = "sexo")

# Calculo de los beneficios por edad, genero y año -------------------------------------------------
ben_proy_edad_sexo <- pob_proy_edad_sexo %>%
  mutate(
    ben_p1 = lp1 * alp1 * 0.7 * sbu,
    ben_p2 = lp2 * alp2 * 0.7 * sbu,
    ben_p3 = lp3 * alp3 * 0.7 * sbu,
    ben_p4 = lp4 * alp4 * 0.7 * sbu,
    ben_p5 = lp5 * alp5 * 0.7 * sbu
  ) %>%
  arrange(t, sexo, x)

# Calculo de los beneficios por año ----------------------------------------------------------------
ben_proy <- ben_proy_edad_sexo %>%
  group_by(t) %>%
  mutate(
    l2_cot = sum(l2_cot, na.rm = TRUE),
    l2_ces = sum(l2_ces, na.rm = TRUE),
    lp1 = sum(lp1, na.rm = TRUE),
    lp2 = sum(lp2, na.rm = TRUE),
    lp3 = sum(lp3, na.rm = TRUE),
    lp4 = sum(lp4, na.rm = TRUE),
    lp5 = sum(lp5, na.rm = TRUE),
    ben_p1 = sum(ben_p1, na.rm = TRUE),
    ben_p2 = sum(ben_p2, na.rm = TRUE),
    ben_p3 = sum(ben_p3, na.rm = TRUE),
    ben_p4 = sum(ben_p4, na.rm = TRUE),
    ben_p5 = sum(ben_p5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(t, .keep_all = TRUE) %>%
  select(-sexo, -x, -alp1, -alp2, -alp3, -alp4, -alp5) %>%
  arrange(t)

# Calculado beneficios de desempleo con pagos indebidos --------------------------------------------
# Preparando datos ---------------------------------------------------------------------------------
mod_benficio_des_pagos_ind <- dcast(mod_benficio_des_pagos_ind, sexo ~ pago, value.var = "alpha")
pob_proy_edad_sexo_pagos_ind <- left_join(pob_proy_edad_sexo_pagos_ind, sbu_proy, by = c("t")) %>%
  left_join(mod_benficio_des_pagos_ind, by = "sexo")

# Calculo de los beneficios por edad, genero y año -------------------------------------------------
ben_proy_edad_sexo_pagos_ind <- pob_proy_edad_sexo_pagos_ind %>%
  mutate(
    ben_p1 = lp1 * alp1 * 0.7 * sbu,
    ben_p2 = lp2 * alp2 * 0.7 * sbu,
    ben_p3 = lp3 * alp3 * 0.7 * sbu,
    ben_p4 = lp4 * alp4 * 0.7 * sbu,
    ben_p5 = lp5 * alp5 * 0.7 * sbu
  ) %>%
  arrange(t, sexo, x)

# Calculo de los beneficios por año ----------------------------------------------------------------
ben_proy_pagos_ind <- ben_proy_edad_sexo_pagos_ind %>%
  group_by(t) %>%
  mutate(
    l2_cot = sum(l2_cot, na.rm = TRUE),
    l2_ces = sum(l2_ces, na.rm = TRUE),
    lp1 = sum(lp1, na.rm = TRUE),
    lp2 = sum(lp2, na.rm = TRUE),
    lp3 = sum(lp3, na.rm = TRUE),
    lp4 = sum(lp4, na.rm = TRUE),
    lp5 = sum(lp5, na.rm = TRUE),
    ben_p1 = sum(ben_p1, na.rm = TRUE),
    ben_p2 = sum(ben_p2, na.rm = TRUE),
    ben_p3 = sum(ben_p3, na.rm = TRUE),
    ben_p4 = sum(ben_p4, na.rm = TRUE),
    ben_p5 = sum(ben_p5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct(t, .keep_all = TRUE) %>%
  select(-sexo, -x, -alp1, -alp2, -alp3, -alp4, -alp5) %>%
  arrange(t)

# Guardando Rdata ----------------------------------------------------------------------------------
message("\tGuardando la estimación de beneficios de desempleo")
save(ben_proy_edad_sexo, ben_proy, ben_proy_edad_sexo_pagos_ind, ben_proy_pagos_ind,
  file = paste0(parametros$RData_seg, "IESS_DES_proyeccion_beneficios.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros", "Hipotesis"))])
gc()
