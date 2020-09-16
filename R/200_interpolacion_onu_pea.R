message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'ONU_pea_ecu.RData' ) )

message( '\tPreparando datos PEA' )
onu_pea_tot <- onu_pea_tot[ sex != 'FM' ]

onu_pea_tot[ , x_min := gsub( "[+]","", gsub( pattern = "[ ]+-[ ]+[0-9]{1,2}$", replacement = "",
                                              x = str_trim( x ) ) ) ]
onu_pea_tot[ , x_max := gsub( "[+]","", gsub( pattern = "^[0-9]{1,2}[ ]+-[ ]+", replacement = "", 
                                              x = str_trim( x ) ) ) ]
onu_pea_tot[ x == "100+", x_min := "100" ]
onu_pea_tot[ x == "100+", x_max := "100" ]
onu_pea_tot <- onu_pea_tot[ x != 'total' ]

onu_pea_tot[ , x_min := as.numeric(x_min)]
onu_pea_tot[ , x_max := as.numeric(x_max)]

setorder( onu_pea_tot, country, sex, year, x_min )

onu_pea_tot[ , peas := shift( pea, n = 1, type = "lead"), by = list( country, sex, year) ]
onu_pea_tot[ , x_max := x_max + 1 ]

onu_pea_tot[ , r := x_max - x_min ]

onu_pea_tot_int <- onu_pea_tot[ , list( pea = rep( pea, r ),
                                        peas = rep( peas, r ),
                                        x = seq( x_min, x_max-1, 1 ) ), 
                                by = list( country, sex, year, x_min, x_max )  ]

# Función para resolver las integraciones locales
solve_int<-function( x, x_min, x_max, lx, lxs ) {
  
  X<-x
  X1<-X+1
  xn<-x_min
  xn1<-x_max
  xn2<-xn1 + 5
  x12<-( xn + xn1 ) / 2
  x32<-( xn1 + xn2 ) / 2
  yn<-lx
  yn1<-lxs
  
  A<-matrix( c( ( xn1^3 - xn^3 ) / 3, ( xn1^2 - xn^2 ) / 2, xn1 - xn,
                ( x32^3 - x12^3 ) / 3, ( x32^2 - x12^2 ) / 2, x32 - x12,
                ( xn2^3 - xn^3 ) / 3, ( xn2^2 - xn^2 ) / 2, xn2 - xn ), 3, 3, byrow = TRUE )
  b<-c( yn, ( yn + yn1 ) / 2, yn + yn1 )
  a<-solve( A, b )
  
  l<-sum( c( ( X1^3 - X^3 ) / 3, ( X1^2 - X^2 ) / 2, X1 - X ) * a )
  
  return( l )
}

message( '\tInterpolando datos PEA' )
# Interpolación cuadrática para preservar el tamaño de la población---------------------------------
onu_pea_tot_int[ x < 100, pea_int := mapply( FUN = solve_int, x, x_min, x_max, pea, peas ), by = list( sex, year ) ]
onu_pea_tot_int[ x >= 100, pea_int := pea ]

setorder( onu_pea_tot_int, country, sex, x, year )
onu_pea_tot_int[ , pea_int_s := shift( pea_int, type = 'lag' ), by = list( country, sex, x ) ]
onu_pea_tot_int[ , log_r := log( pea_int / pea_int_s ) ]

rate_pea_mod_f <- lm( log_r ~ bs( x, df = 50, degree = 1 )  +  year,
                      data = onu_pea_tot_int[ sex == 'F' & year >= 1980 & is.finite( log_r ) ] )

rate_pea_mod_m <- lm( log_r ~ bs( x, df = 50, degree = 1 )  +  year,
                      data = onu_pea_tot_int[ sex == 'M' & year >= 1980 & is.finite( log_r ) ] )

onu_pea_tot_proy_m <- data.table( expand.grid( x = 15:100, year = 2051:2058 ) )
onu_pea_tot_proy_f <- data.table( expand.grid( x = 15:100, year = 2051:2058 ) )

onu_pea_tot_proy_m[ , log_r := predict( object = rate_pea_mod_m, newdata = onu_pea_tot_proy_m ) ]
onu_pea_tot_proy_m[ , sex := 'M' ]
onu_pea_tot_proy_f[ , log_r := predict( object = rate_pea_mod_f, newdata = onu_pea_tot_proy_f ) ]
onu_pea_tot_proy_f[ , sex := 'F' ]

onu_pea_tot_proy <- rbind( onu_pea_tot_proy_f, onu_pea_tot_proy_m )

aux_proy <- NULL
aux_last <- copy( onu_pea_tot_int[ sex != 'FM' & year == 2050 ] )
for ( i in 1:8 ) {
  aux <- copy( aux_last )
  aux[ , year := 2050 + i ]
  aux_proy <- rbind( aux_proy, aux )
}
aux_proy[ , log_r := NULL ]
aux_proy <- merge( aux_proy, onu_pea_tot_proy, by = c( 'sex', 'x', 'year' ) )
setorder( aux_proy, sex, x, year )
aux_proy[ , r := exp( log_r ) ]
aux_proy[ , r := cumprod( r ), by = list( sex, x ) ]
aux_proy[ , pea_int := r * pea_int ]

onu_pea_tot_int <- rbind( onu_pea_tot_int[ , list( year, country, sex, x_min, x_max, pea, x, pea_int, log_r ) ], 
                          aux_proy[ , list( year, country, sex, x_min, x_max, pea, x, pea_int, log_r ) ] )

setorder( onu_pea_tot_int, country, year, sex, x )
# onu_pea_tot_proy[ , r := exp( log_r ) ]
# Verificación interporlación
# check_int <- onu_pea_tot_int[ , list( pea = mean( pea, na.rm = TRUE ),
#                                       pea_int = sum( pea_int, na.rm = TRUE ) ),
#                               by = list( country, year, sex, x_min )]
# chk <- check_int[ abs( pea - pea_int )  > 1 ]
# 
# check_int <- onu_pea_tot_int[ , list( pea_int = sum( pea_int, na.rm = TRUE ) ),
#                               by = list( country, year, sex ) ]
# setorder( check_int, sex, year )
# 
# check_int <- aux_proy[ , list( pea_int = sum( pea_int, na.rm = TRUE ) ),
#                               by = list( country, year ) ]

# aux_f <- onu_pea_tot_int[ sex == 'M' & year >= 2020 ]
# 
# plt <- ggplot() +
#   geom_line( data = aux_f, aes( x = x, y = pea_int, col = as.character( year ) ) ) +
#   xlab( 'Monto' ) +
#   ylab( '' ) +
#   theme_bw() +
#   theme( legend.position = 'none' )
# plt

save( onu_pea_tot_int, 
      file = paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
