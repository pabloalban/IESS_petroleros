message( paste( rep('-', 100 ), collapse = '' ) )

# Carga --------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_tasa_actividad.RData' ) )

tasa_act[ , ER := desempleo + actividad ]
tasa_act[ ER > 0, tau_est := actividad / ER ]
tasa_act[ ER == 0, tau_est := 0 ]
tasa_act[ , log_tau_est := log( tau_est ) ]

tasa_act_proy_f <- data.table( x = 15:100 )
tau_smooth_model_f <- lm( log_tau_est ~ bs( x, df = 3, degree = 3 ), 
                          data = tasa_act[ sexo == 'F' & x >= 18 & x <= 85 & is.finite( log_tau_est ) ] )

tasa_act_proy_f[ , log_tau := predict( object = tau_smooth_model_f, newdata = tasa_act_proy_f ) ]
tasa_act_proy_f[ , sexo := 'F' ]

tasa_act_proy_m <- data.table( x = 15:100 )
tau_smooth_model_m <- lm( log_tau_est ~ bs( x, df = 3, degree = 3 ), 
                          data = tasa_act[ sexo == 'M' & x >= 18 & x <= 86 & is.finite( log_tau_est ) ] )

tasa_act_proy_m[ , log_tau := predict( object = tau_smooth_model_m, newdata = tasa_act_proy_m ) ]
tasa_act_proy_m[ , sexo := 'M' ]

tasa_act_proy <- rbind( tasa_act_proy_f, tasa_act_proy_m )
tasa_act_proy <- merge( tasa_act, tasa_act_proy, by = c( 'sexo', 'x' ) )
tasa_act_proy[ , tau := exp( log_tau ) ]
setorder( tasa_act_proy, sexo, x )

# par( mfrow = c( 1, 2 ) )
# aux_f <- tasa_act_proy[ sexo == 'F' ]
# plot( aux_f$x, aux_f$log_tau_est, type = 'p', pch = 16, cex = 0.8, col = parametros$iess_green, ylim = c( -1, 0 ) )
# points( aux_f$x, aux_f$log_tau, type = 'l', pch = 16, cex = 0.8, col = parametros$iess_blue )
# 
# aux_m <- tasa_act_proy[ sexo == 'M' ]
# plot( aux_m$x, aux_m$log_tau_est, type = 'p', pch = 16, cex = 0.8, col = parametros$iess_green, ylim = c( -1, 0 ) )
# points( aux_m$x, aux_m$log_tau, type = 'l', pch = 16, cex = 0.8, col = parametros$iess_blue )

# Guardando ----------------------------------------------------------------------------------------
message( '\tGuardando tasa de mortalidad de pensionistas' )
save( tasa_act_proy,
      tau_smooth_model_f, tau_smooth_model_m,
      file = paste0( parametros$RData, 'IESS_estimacion_tasa_actividad.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
