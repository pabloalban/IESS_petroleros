message( paste( rep('-', 100 ), collapse = '' ) )

# Carga --------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_entradas_afiliados_v2.RData' ) )
load( paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )

# Estimación --------------------------------------------------------------------------------------
message( '\tEstimando tasa de entradas' )
tasa_ent <- merge( ent_afi, 
                   onu_pea_tot_int[ ,list( anio = year, sexo = sex, x, pea = pea_int ) ],
                   by = c( 'anio', 'sexo', 'x' ) )
setorder( tasa_ent, anio, sexo, x )
tasa_ent[ is.na( ent ), ent := 0 ]
tasa_ent[ is.na( pea ), pea := 0 ]
tasa_ent[ , ue_est := ent / pea ]
tasa_ent[ , log_ue_est := log( ue_est ) ]

tasa_ent_esp <- tasa_ent[ , list( ue_est = mean( ue_est, na.rm = TRUE ),
                                  ER = mean( pea, na.rm = TRUE ),
                                  N = mean( ent, na.rm = TRUE ) ), 
                          by = list( sexo, x ) ]
tasa_ent_esp[ , ue_est := N / ER ]
tasa_ent_esp[ , log_ue_est := log( ue_est ) ]

tas_proj_f <- data.table( x = 15:100 )
tas_proj_m <- data.table( x = 15:100 )

# Alisando ----------------------------------------------------------------------------------------
message( '\tAlisando tasa' )
ue_smooth_model_f <- lm( log_ue_est ~ bs( x, df = 8, degree = 3 ), 
                         weights = ER,
                         data = tasa_ent_esp[ sexo == 'F' & is.finite( log_ue_est ) & x <= 80 ] )

tas_proj_f[ , log_ue := predict( object = ue_smooth_model_f, 
                                 newdata = tas_proj_f[ , list( x ) ] ) ]

ue_smooth_model_m <- lm( log_ue_est ~ bs( x, df = 8, degree = 3 ),
                         weights = ER,
                         data = tasa_ent_esp[ sexo == 'M' & is.finite( log_ue_est ) & x <= 80 ] )

tas_proj_m[ , log_ue := predict( object = ue_smooth_model_m, 
                                 newdata = tas_proj_m[ , list( x ) ] ) ]

tas_proj_f[ , sexo := 'F' ]
tas_proj_m[ , sexo := 'M' ]

aux <- rbind( tas_proj_f, tas_proj_m )
tasa_ent_esp <- merge( tasa_ent_esp, aux, by = c( 'sexo', 'x' ), all.y = TRUE )
tasa_ent_esp[ , ue := exp( log_ue ) ]

# Guardando --------------------------------------------------------------------------------------
message( '\tGuardando tasa de entradas' )
save( tasa_ent, tasa_ent_esp, 
      ue_smooth_model_f, ue_smooth_model_m,
      file = paste0( parametros$RData, 'IESS_estimacion_tasa_entradas.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
