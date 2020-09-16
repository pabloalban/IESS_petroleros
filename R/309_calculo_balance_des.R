message(paste(rep("-", 100), collapse = ""))

load(paste0(parametros$RData_seg, "IESS_DES_proyeccion_poblacion.RData"))
load(paste0(parametros$RData_seg, "IESS_DES_proyeccion_beneficios.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_proyeccion_salarios.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData"))

# Borrando variables, solo quedan variables a ser utilizadas
rm(list = ls()[!(ls() %in% c(
  parametros_lista,
  "pob_proy_edad_sexo",
  "ben_proy_edad_sexo_pagos_ind",
  "sal_proy",
  "ben_proy_edad_sexo", "densidad_cotizacion_int"
))])

message("\tCalculando escenario: ", esc$nombre)

# Escenarios con pagos indebidos---------------------------------------------------------------------
if (esc$nombre == "escenario_2") {
  aux <- as.data.table(ben_proy_edad_sexo_pagos_ind)
  # } else if (esc$nombre=="escenario_3") {
  # aux=as.data.table(ben_proy_edad_sexo_pagos_ind)
} else {
  aux <- as.data.table(ben_proy_edad_sexo)
}

# Calibración de beneficios-------------------------------------------------------------------------
cal_benf_des <- esc$calibra_benef

# Balance corriente --------------------------------------------------------------------------------
message("\tGenerando balance corriente")
# ben_proy_edad_sexo<-as.data.table(ben_proy_edad_sexo)
balance <- merge(aux,
  sal_proy[, list(t, sexo, x, sal = sal)],
  by = c("t", "sexo", "x")
)

balance <- balance[, list(
  t, sexo, x,
  l2_cot_sgo, l2_cot, l2_ces,
  lp1, lp2, lp3, lp4, lp5,
  i_sbu, sbu,
  alp1, alp2, alp3, alp4, alp5,
  ben_p1, ben_p2, ben_p3, ben_p4, ben_p5, sal
)]

setorder(balance, t, sexo, x)

message("\tProyectando masa salarial")
balance <- as.data.table(balance)
balance[, M := esc$calibra_apo * sal * l2_cot]

# Beneficios de las prestaciones de la parte fija
message("\tProyectando beneficios por pago de prestaciones")
balance$ben_p1 <- cal_benf_des * balance$ben_p1
balance$ben_p2 <- cal_benf_des * balance$ben_p2
balance$ben_p3 <- cal_benf_des * balance$ben_p3
balance$ben_p4 <- cal_benf_des * balance$ben_p4
balance$ben_p5 <- cal_benf_des * balance$ben_p5

balance[, B := ben_p1 + ben_p2 + ben_p3 + ben_p4 + ben_p5]

message("\tProyectando aportes")
# Aportes de los cotizantes activos a desempleo
balance <- merge(balance, esc$apo_act, by = "t", all.x = TRUE)
balance[t == 0, A := 0]
balance[, A := por_apo * M]

# Gasto administrativo
balance[, G := esc$porcentaje_gasto * M]
balance[t == 0, G := 0]

# No hay contribución del estado en desempleo

# Balance corriente
balance_anual <- balance[, list(
  M = sum(M),
  A = sum(A),
  B = sum(B),
  B1 = sum(ben_p1),
  B2 = sum(ben_p2),
  B3 = sum(ben_p3),
  B4 = sum(ben_p4),
  B5 = sum(ben_p5),
  G = sum(G),
  l2_cot = sum(l2_cot)
),
by = list(t)
]

balance_anual[, i_a := esc$i_a]
balance_anual[, r := (1 + i_a)^(t)]
balance_anual[, v := (1 + i_a)^(-t)]
balance_anual[, V_cor := A - B - G]
balance_anual[t == 0, `:=`(
  M = 0,
  A = 0,
  B = 0,
  B1 = 0,
  B2 = 0,
  B3 = 0,
  B4 = 0,
  B5 = 0,
  G = 0, 
  V_cor = 0
)]
balance_anual[, V_cap := V_cor]
balance_anual[t == 0, V_cap := esc$V0]
balance_anual[, V_cap := r * cumsum(v * V_cap)]

# Balance actuarial---------------------------------------------------------------------------------
balance_anual[, M_vap := cumsum(v * M)]

# Ingresos
balance_anual[, A_vap := cumsum(v * A)]

# Egresos
balance_anual[, B_vap := cumsum(v * B)]
balance_anual[, B1_vap := cumsum(v * B1)]
balance_anual[, B2_vap := cumsum(v * B2)]
balance_anual[, B3_vap := cumsum(v * B3)]
balance_anual[, B4_vap := cumsum(v * B4)]
balance_anual[, B5_vap := cumsum(v * B5)]
balance_anual[, G_vap := cumsum(v * G)]

# Balance
balance_anual[, V := v * V_cap]
balance_anual[, V0 := esc$V0]

# Guardando balances -------------------------------------------------------------------------------
message("\tGuardando balances")
save(balance, balance_anual,
  file = paste0(parametros$RData_seg, "IESS_DES_balances_", esc$nombre, ".RData")
)

message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c(parametros_lista, "parametros"))])
gc()
