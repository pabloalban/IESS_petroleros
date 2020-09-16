message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# Escenario 1 --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
# load( paste0( parametros$RData_seg, 'IESS_DES_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_DES_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_DES_balances_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$tasa_aporte_esc_1<-format( 100 * esc$apo_act[1,2],
#                                digits = 2, nsmall = 2, big.mark = '.', 
#                                decimal.mark = ',', format = 'f' )
# 
# REP$tasa_act_esc_1<-format( 100 * esc$i_a,
#                             digits = 2, nsmall = 2, big.mark = '.', 
#                             decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_1 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1
# 
# REP$cap_ini <- format( esc$V0, 
#                        digits = 2, nsmall = 2, big.mark = '.', 
#                        decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo,
#                                  digits = 4, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$tasa_porcentaje_gasto_esc_1<-format( 100 * esc$porcentaje_gasto,
#                                          digits = 2, nsmall = 2, big.mark = '.', 
#                                          decimal.mark = ',', format = 'f' )
# 
# REP$bal_sum_act_1 <- format( balance_anual[ t == parametros$horizonte ]$A_vap+esc$V0, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_sum_pas_1 <- format( balance_anual[ t == parametros$horizonte ]$B_vap+balance_anual[ t == parametros$horizonte ]$G_vap, 
#                                 digits = 2, nsmall = 2, big.mark = '.', 
#                                 decimal.mark = ',', format = 'f' )
# 
