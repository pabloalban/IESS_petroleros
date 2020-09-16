message( paste( rep('-', 100 ), collapse = '' ) )

# Carga --------------------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_IVM_mortalidad_pensionistas.RData' ) )

# Estimación vejez ---------------------------------------------------------------------------------
message( '\tEstimando tasa de mortalidad de pensionistas de vejez' )
tasa_mort_vej <- copy( mort_pen_vej )
tasa_mort_vej[ ER > 0, udv_est := N / ER ]
tasa_mort_vej[ ER == 0, udv_est := 0 ]

tasa_mort_vej <- tasa_mort_vej[ , list( udv_est_1 = mean( udv_est, na.rm = TRUE ),
                                        ER = sum( ER, na.rm = TRUE ),
                                        N = sum( N, na.rm = TRUE ) ), 
                                by = list( sexo, x ) ]
tasa_mort_vej[ ER > 0, udv_est_2 := N / ER ]
tasa_mort_vej[ ER == 0, udv_est_2 := 0 ]
tasa_mort_vej[ , log_udv_est := log( udv_est_2 ) ]

# plot( tasa_mort_vej$x, tasa_mort_vej$udv_est_1, pch = 16, cex = 0.8, col = parametros$iess_green )
# points( tasa_mort_vej$x, tasa_mort_vej$udv_est_2, pch = 16, cex = 0.8, col = parametros$iess_blue )

# Alisando vejez -----------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad pensionistas de vejez' )
tasa_mort_vej_proy_f <- data.table( x = 48:105 )
udv_smooth_model_f <- lm( log_udv_est ~ bs( x, df = 4, degree = 3, Boundary.knots = range( tasa_mort_vej_proy_f$x ) ), 
                          weights = ER,
                          data = tasa_mort_vej[ sexo == 'F' & is.finite( log_udv_est ) & x >= 53 ] )

tasa_mort_vej_proy_f[ , log_udv := predict( object = udv_smooth_model_f, 
                                            newdata = tasa_mort_vej_proy_f ) ]
tasa_mort_vej_proy_f[ , sexo := 'F' ]

tasa_mort_vej_proy_m <- data.table( x = 48:105 )
udv_smooth_model_m <- lm( log_udv_est ~ bs( x, df = 4, degree = 3, Boundary.knots = range( tasa_mort_vej_proy_m$x ) ), 
                          weights = ER,
                          data = tasa_mort_vej[ sexo == 'M' & is.finite( log_udv_est ) & x >= 53 ] )

tasa_mort_vej_proy_m[ , log_udv := predict( object = udv_smooth_model_m, 
                                            newdata = tasa_mort_vej_proy_m ) ]
tasa_mort_vej_proy_m[ , sexo := 'M' ]

tasa_mort_vej_proy <- rbind( tasa_mort_vej_proy_f, tasa_mort_vej_proy_m )
tasa_mort_vej_proy <- merge( tasa_mort_vej, tasa_mort_vej_proy, by = c( 'sexo', 'x' ) )

# aux_f <- tasa_mort_vej_proy[ sexo == 'F' ]
# plot( aux_f$x, aux_f$log_udv_est, cex = 0.7, pch = 16, col = parametros$iess_blue, ylim = c( -10, 0 ) )
# points( aux_f$x, aux_f$log_udv, type = 'l', col = parametros$iess_green )
# 
# aux_m <- tasa_mort_vej_proy[ sexo == 'M' ]
# plot( aux_m$x, aux_m$log_udv_est, cex = 0.7, pch = 16, col = parametros$iess_blue, ylim = c( -10, 0 ) )
# points( aux_m$x, aux_m$log_udv, type = 'l', col = parametros$iess_green )


# Estimación invalidez -----------------------------------------------------------------------------
message( '\tEstimando tasa de mortalidad de pensionistas de invalidez' )
tasa_mort_inv <- copy( mort_pen_inv )
tasa_mort_inv[ ER > 0, udi_est := N / ER ]
tasa_mort_inv[ ER == 0, udi_est := 0 ]

tasa_mort_inv <- tasa_mort_inv[ , list( udi_est_1 = mean( udi_est, na.rm = TRUE ),
                                        ER = sum( ER, na.rm = TRUE ),
                                        N = sum( N, na.rm = TRUE ) ), 
                                by = list( sexo, x ) ]
tasa_mort_inv[ ER > 0, udi_est_2 := N / ER ]
tasa_mort_inv[ ER == 0, udi_est_2 := 0 ]
tasa_mort_inv[ , log_udi_est := log( udi_est_1 ) ]

# plot( tasa_mort_inv$x, tasa_mort_inv$udi_est_1, pch = 16, cex = 0.8, col = parametros$iess_green )
# points( tasa_mort_inv$x, tasa_mort_inv$udi_est_2, pch = 16, cex = 0.8, col = parametros$iess_blue )

# Alisando invalidez -------------------------------------------------------------------------------
message( '\tAlisando tasa mortalidad pensionistas de invalidez' )
tasa_mort_inv_proy_f <- data.table( x = 20:105 )
udi_smooth_model_f <- lm( log_udi_est ~ bs( x, df = 6, degree = 3, Boundary.knots = range( tasa_mort_inv_proy_f$x ) ), 
                          weights = ER,
                          data = tasa_mort_inv[ sexo == 'F' & is.finite( log_udi_est ) & 
                                                  x >= 18 & x <= 95 ] )

tasa_mort_inv_proy_f[ , log_udi := predict( object = udi_smooth_model_f, 
                                            newdata = tasa_mort_inv_proy_f ) ]
tasa_mort_inv_proy_f[ , sexo := 'F' ]

tasa_mort_inv_proy_m <- data.table( x = 20:105 )
udi_smooth_model_m <- lm( log_udi_est ~ bs( x, df = 6, degree = 3, Boundary.knots = range( tasa_mort_inv_proy_m$x ) ), 
                          weights = ER,
                          data = tasa_mort_inv[ sexo == 'M' & is.finite( log_udi_est ) & 
                                                  x >= 18 & x <= 95 ] )

tasa_mort_inv_proy_m[ , log_udi := predict( object = udi_smooth_model_m, 
                                            newdata = tasa_mort_inv_proy_m ) ]
tasa_mort_inv_proy_m[ , sexo := 'M' ]

tasa_mort_inv_proy <- rbind( tasa_mort_inv_proy_f, tasa_mort_inv_proy_m )
tasa_mort_inv_proy <- merge( tasa_mort_inv, tasa_mort_inv_proy, by = c( 'sexo', 'x' ) )

tasa_mort_inv_proy[ , log_udi_mean := mean( log_udi ), by = list( x ) ]
tasa_mort_inv_proy[ x <= 45, log_udi := log_udi_mean, by = list( sexo, x ) ]

# aux_f <- tasa_mort_inv_proy[ sexo == 'F' ]
# plot( aux_f$x, aux_f$log_udi_est, cex = 0.7, pch = 16, col = parametros$iess_blue, ylim = c( -10, 0 ) )
# points( aux_f$x, aux_f$log_udi, type = 'l', col = parametros$iess_green )
# 
# aux_m <- tasa_mort_inv_proy[ sexo == 'M' ]
# plot( aux_m$x, aux_m$log_udi_est, cex = 0.7, pch = 16, col = parametros$iess_blue, ylim = c( -10, 0 ) )
# points( aux_m$x, aux_m$log_udi, type = 'l', col = parametros$iess_green )

# Guardando ----------------------------------------------------------------------------------------
message( '\tGuardando tasa de mortalidad de pensionistas' )
save( tasa_mort_vej_proy,
      udv_smooth_model_f, udv_smooth_model_m,
      tasa_mort_inv_proy,
      udi_smooth_model_f, udi_smooth_model_m,
      file = paste0( parametros$RData, 'IESS_estimacion_tasa_mortalidad_pensionistas.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
