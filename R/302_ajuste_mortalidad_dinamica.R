message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'ONU_interpolado_life_table_survivors_2019.RData' ) )
load( paste0( parametros$RData, 'IESS_tabla_decrementos.RData' ) )
load( paste0( parametros$RData, 'IESS_estimacion_tasa_mortalidad_pensionistas.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'tab_dec', 'onu_ecu_mort_din', 'tasa_mort_vej_proy',
                                 'tasa_mort_inv_proy' ) ) ] )

aux_onu <- onu_ecu_mort_din[ t >= 2018, list( t, sexo, x, q_onu_x = qx, vx ) ]
aux_iess <- tab_dec[ , list( t = 2018, sexo, x, qx = 1 - exp( -ud ) ) ]
iess_mort_din <- merge( aux_onu, 
                        aux_iess[ , list( sexo, x, qx  ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        tasa_mort_vej_proy[ , list( sexo, x, qvx = 1 - exp( -exp( log_udv ) ) ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

iess_mort_din <- merge( iess_mort_din, 
                        tasa_mort_inv_proy[ , list( sexo, x, qix = 1 - exp( -exp( log_udi ) ) ) ], 
                        by = c( 'sexo', 'x' ), all.x = TRUE )

setorder( iess_mort_din, t, sexo, x )

message( '\tAjustando probabilidad por variaciones' )
iess_mort_din[ is.na( qx ), qx := q_onu_x ] # solución temporal hasta completar la tabla del IESS
iess_mort_din[ is.na( qvx ), qvx := 0 ]
iess_mort_din[ is.na( qix ), qix := 0 ]
iess_mort_din[ t == 2018, vx := 1 ]
iess_mort_din[ , qx := vx * qx ]
iess_mort_din[ , qvx := vx * qvx ]
iess_mort_din[ , qix := vx * qix ]
iess_mort_din[ , px := 1 - qx ] 
iess_mort_din[ , pvx := 1 - qvx ] 
iess_mort_din[ , pix := 1 - qix ] 
iess_mort_din[ , ux := -log( px ) ]
iess_mort_din[ , uvx := -log( pvx ) ]
iess_mort_din[ , uix := -log( pix ) ]
iess_mort_din[ , log_ux := log( ux ) ]
iess_mort_din[ , log_uvx := log( uvx ) ]
iess_mort_din[ , log_uix := log( uix ) ]

message( '\tGuardando resultados' )
save( iess_mort_din, 
      file = paste0( parametros$RData, 'IESS_tabla_mortalidad_dinamica.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
