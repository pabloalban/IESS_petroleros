message( paste( rep('-', 100 ), collapse = '' ) )

# Descripción estados:
# 1 = Individuos en la PEA no activos
# 2 = Activos
# 2_cot = Activos cotizantes
# 2_ces = Activos cesantes
# 3 = Pensionistas de vejez
# 4 = Pensionistas de invalidez
# 5 = Muertos
# 6 = Pensionistas de montepío
# 7 = Hijos de cotizantes
# 8 = Cónjuges no asegurados de cotizantes

# Cargando información -----------------------------------------------------------------------------
message( '\tCargando datos' )
load( paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )
load( paste0( parametros$RData, 'IESS_poblacion_inicial.RData' ) )
load( paste0( parametros$RData, 'IESS_probabilidades_transicion.RData' ) )
load( paste0( parametros$RData, 'IESS_estimacion_tasa_actividad.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'tasa_act_proy', 
                                 'onu_pea_tot_int', 'pob_ini', 
                                 'Pf', 'Pm', 'tau_f', 'tau_m' ) ) ] )

# Horizonte de proyección
t_horiz <- parametros$horizonte

# Año inicial de proyección
fec_ini <- parametros$anio_ini

# Año final de proyección
fec_fin <- fec_ini + t_horiz

# Tiempo
t <- 0:t_horiz

# Edades
x_max <- parametros$edad_max
x <- 0:x_max

N <- length( t )
M <- length( x )

# Arrays para población
lf <- array( 0.0, dim = c( M, N, 5 ) )
lm <- array( 0.0, dim = c( M, N, 5 ) )

# Conteos de transición
ltf <- array( 0.0, dim = c( M, N, 11 ) )
ltm <- array( 0.0, dim = c( M, N, 11 ) )

# PEA ----------------------------------------------------------------------------------------------
message( '\tPreparando población económicamente activa' )
PEA_f <- onu_pea_tot_int[ sex == 'F' & year >= fec_ini & year <= fec_fin & x <= x_max, 
                          list( t = year - fec_ini, x, pea = pea_int ) ]
PEA_f[ , pea := ( pmin( 0.7 * ( 1.03566749 )^t, 1 ) ) * pea ] # calibrando crecimiento PEA ver ENEMDU 2018
PEA_f <- merge( data.table( expand.grid( t = t, x = x ) ),
                PEA_f, by = c( 't', 'x' ), all.x = TRUE )
PEA_f[ is.na( pea ), pea := 0 ]
setorder( PEA_f, t, x )
PEA_f <- dcast.data.table( data = PEA_f, x ~ t, value.var = 'pea' )
PEA_f <- as.matrix( PEA_f[ , 2:ncol( PEA_f ) ] )

PEA_m <- onu_pea_tot_int[ sex == 'M' & year >= fec_ini & year <= fec_fin & x <= x_max, 
                          list( t = year - fec_ini, x, pea = pea_int ) ]
PEA_m[ , pea := ( pmin( 0.7 * ( 1.03566749 )^t, 1 ) ) * pea ] # calibrando crecimiento PEA ver ENEMDU 2018
PEA_m <- merge( data.table( expand.grid( t = t, x = x ) ),
                PEA_m, by = c( 't', 'x' ), all.x = TRUE )
PEA_m[ is.na( pea ), pea := 0 ]
setorder( PEA_m, t, x )
PEA_m <- dcast.data.table( data = PEA_m, x ~ t, value.var = 'pea' )
PEA_m <- as.matrix( PEA_m[ , 2:ncol( PEA_m ) ] )

# Población inicial --------------------------------------------------------------------------------
message( '\tPreparando población inicial' )
l0_f <- pob_ini[ sexo == 'F', list( x, estado, lx ) ]
setorder( l0_f, x )
l0_f[ estado == 'afi', est := 2 ]
l0_f[ estado == 'pvej', est := 3 ]
l0_f[ estado == 'pinv', est := 4 ]
l0_f <- dcast.data.table( data = l0_f, formula = x ~ est, value.var = 'lx' )
l0_f <- cbind( l0_f, rep( 0, M ) )
l0_1_f <- PEA_f[ , 1 ]
l0_f <- cbind( l0_f, l0_1_f )
setnames( l0_f, c( 'x', paste0( 'l', c( 2:5, 1 ) ) ) )
l0_f <- cbind( l0_f, tau_f )
# transformando población activa a afiliada
l0_f[ tau > 0, l2 := l2 / tau ] 
l0_f[ , l1 := l1 - l2 ] 
l0_f <- as.matrix( l0_f[ , c( 6, 2, 3, 4, 5 ) ] )

l0_m <- pob_ini[ sexo == 'M', list( x, estado, lx ) ]
setorder( l0_m, x )
l0_m[ estado == 'afi', est := 2 ]
l0_m[ estado == 'pvej', est := 3 ]
l0_m[ estado == 'pinv', est := 4 ]
l0_m <- dcast.data.table( data = l0_m, formula = x ~ est, value.var = 'lx' )
l0_m <- cbind( l0_m, rep( 0, M ) )
l0_1_m <- PEA_m[ , 1 ]
l0_m <- cbind( l0_m, l0_1_m )
setnames( l0_m, c( 'x', paste0( 'l', c( 2:5, 1 ) ) ) )
l0_m <- cbind( l0_m, tau_m )
# transformando población activa a afiliada
l0_m[ tau > 0, l2 := l2 / tau ]
l0_m[ , l1 := l1 - l2 ]
l0_m <- as.matrix( l0_m[ , c( 6, 2, 3, 4, 5 ) ] )

rm( l0_1_f, l0_1_m )

# Parámetros iniciales -----------------------------------------------------------------------------
message( '\tEstableciendo condición inicial' )
lf[ , 1, ] <- l0_f
lm[ , 1, ] <- l0_m

# Proyección población -----------------------------------------------------------------------------
message( '\tProyectando población' )
for ( n in 1:(N-1) ) {
  for ( k in 1:(M-1) ) {
    lf[ k + 1, n + 1, ] <- Pf[ k, n, , ] %*% lf[ k, n, ]
    lm[ k + 1, n + 1, ] <- Pm[ k, n, , ] %*% lm[ k, n, ]
    
    # Ajuste población activa
    lf[ 1, n + 1, 1 ] <- 0
    lm[ 1, n + 1, 1 ] <- 0
    lf[ k + 1, n + 1, 1 ] <- max( PEA_f[ k + 1, n + 1 ] - tau_f[ k + 1, 1 ] * lf[ k + 1, n + 1, 2 ], 0 )
    lm[ k + 1, n + 1, 1 ] <- max( PEA_m[ k + 1, n + 1 ] - tau_m[ k + 1, 1 ] * lm[ k + 1, n + 1, 2 ], 0 )
    
    # Conteos de transiciones
    ltf[ k + 1, n + 1, 1 ] <- Pf[ k, n, 1, 1 ] * lf[ k, n, 1 ]
    ltf[ k + 1, n + 1, 2 ] <- Pf[ k, n, 2, 1 ] * lf[ k, n, 1 ]
    ltf[ k + 1, n + 1, 3 ] <- Pf[ k, n, 5, 1 ] * lf[ k, n, 1 ]
    ltf[ k + 1, n + 1, 4 ] <- Pf[ k, n, 2, 2 ] * lf[ k, n, 2 ]
    ltf[ k + 1, n + 1, 5 ] <- Pf[ k, n, 3, 2 ] * lf[ k, n, 2 ]
    ltf[ k + 1, n + 1, 6 ] <- Pf[ k, n, 4, 2 ] * lf[ k, n, 2 ]
    ltf[ k + 1, n + 1, 7 ] <- Pf[ k, n, 5, 2 ] * lf[ k, n, 2 ]
    ltf[ k + 1, n + 1, 8 ] <- Pf[ k, n, 3, 3 ] * lf[ k, n, 3 ]
    ltf[ k + 1, n + 1, 9 ] <- Pf[ k, n, 5, 3 ] * lf[ k, n, 3 ]
    ltf[ k + 1, n + 1, 10 ] <- Pf[ k, n, 4, 4 ] * lf[ k, n, 4 ]
    ltf[ k + 1, n + 1, 11 ] <- Pf[ k, n, 5, 4 ] * lf[ k, n, 4 ]
    
    ltm[ k + 1, n + 1, 1 ] <- Pm[ k, n, 1, 1 ] * lm[ k, n, 1 ]
    ltm[ k + 1, n + 1, 2 ] <- Pm[ k, n, 2, 1 ] * lm[ k, n, 1 ]
    ltm[ k + 1, n + 1, 3 ] <- Pm[ k, n, 5, 1 ] * lm[ k, n, 1 ]
    ltm[ k + 1, n + 1, 4 ] <- Pm[ k, n, 2, 2 ] * lm[ k, n, 2 ]
    ltm[ k + 1, n + 1, 5 ] <- Pm[ k, n, 3, 2 ] * lm[ k, n, 2 ]
    ltm[ k + 1, n + 1, 6 ] <- Pm[ k, n, 4, 2 ] * lm[ k, n, 2 ]
    ltm[ k + 1, n + 1, 7 ] <- Pm[ k, n, 5, 2 ] * lm[ k, n, 2 ]
    ltm[ k + 1, n + 1, 8 ] <- Pm[ k, n, 3, 3 ] * lm[ k, n, 3 ]
    ltm[ k + 1, n + 1, 9 ] <- Pm[ k, n, 5, 3 ] * lm[ k, n, 3 ]
    ltm[ k + 1, n + 1, 10 ] <- Pm[ k, n, 4, 4 ] * lm[ k, n, 4 ]
    ltm[ k + 1, n + 1, 11 ] <- Pm[ k, n, 5, 4 ] * lm[ k, n, 4 ]
    
  }
}

# Transformación proyección a data.table -----------------------------------------------------------
message( '\tCreando data.table con población proyectada y conteos de transición' )
pob_proy <- NULL
tau <- c( tau_f, tau_m )
for ( n in 1:N ) {
  lx <- rbind( lf[ , n, ], lm[ , n, ] )
  ltx <- rbind( ltf[ , n, ], ltm[ , n, ] )
  
  lx <- data.table( t = t[ n ],
                    sexo = rep( c( 'F', 'M' ), each = M ), 
                    x = rep( x, 2 ), 
                    l = lx,
                    lt = ltx,
                    tau = tau )
  
  setnames( lx, c( 't', 'sexo', 'x', 
                   paste0( 'l', 1:5 ), 
                   paste0( 'l', c( '11', '12', '15', '22', '23', '24', '25', '33', '35', '44', '45' ) ), 
                   'tau' ) )
  
  pob_proy <- rbind( pob_proy, lx )
  
}
pob_proy[ , l2_cot := tau * l2 ]
pob_proy[ , l2_ces := ( 1 - tau ) * l2 ]
pob_proy[ , l6 := parametros$mont_prop_afi * ( l3 + l4 ) ]

# Incluyendo proyección de dependientes ------------------------------------------------------------
message( '\tIncluyendo dependientes' )
load( paste0( parametros$RData, 'INEC_censo_iess_fertilidad_alisado_2010.RData' ) )
aux <- cen_iess_hij_alis[ , list( x, z = y, sexo, sexo_dep, qh = q ) ]
aux[ , sexo := as.character( sexo ) ]
aux[ , sexo_dep := as.character( sexo_dep ) ]
aux[ sexo == 'M', sexo := 'F' ]
aux[ sexo == 'H', sexo := 'M' ]
aux[ sexo_dep == 'M', sexo_dep := 'F' ]
aux[ sexo_dep == 'H', sexo_dep := 'M' ]
pob_proy_dep <- merge( pob_proy[ , list( sexo, x, t, l2_cot ) ], 
                       aux, 
                       by = c( 'sexo', 'x' ), 
                       all.x = TRUE, allow.cartesian = TRUE )

pob_proy_dep[ is.na( qh ), qh := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( z ) ]

pob_proy_dep <- merge( pob_proy_dep,
                       cen_iess_cony_alis[ , list( x, y, qc = q ) ], by = c( 'x' ),
                       all.x = TRUE, allow.cartesian = TRUE )
gc()
pob_proy_dep[ is.na( qc ), qc := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( y ) ]
pob_proy_dep[ sexo == 'F', sexo_cony := 'M' ]
pob_proy_dep[ sexo == 'M', sexo_cony := 'F' ]

pob_proy_dep <- merge( pob_proy_dep,
                       aux[ , list( y = x, z, sexo_cony = sexo, sexo_dep, qhc = qh ) ],
                       by = c( 'sexo_cony', 'y', 'sexo_dep', 'z' ) )

pob_proy_dep <- pob_proy_dep[ y - z >= 15 ]
pob_proy_dep <- pob_proy_dep[ y - z <= 50 ]
pob_proy_dep <- pob_proy_dep[ x - z >= 15 ]
pob_proy_dep <- pob_proy_dep[ x - z <= 70 ]
gc()

pob_proy_dep <- pob_proy_dep[ y <= 105, list( l7 = sum( l2_cot * qh * qhc * ( 1 - 0.5 * qc ) ) ),
                              by = list( t, sexo = sexo_dep, x = z ) ]
gc()

pob_proy_cony <- merge( pob_proy[ , list( sexo, x, t, l2_cot ) ], 
                        cen_iess_cony_alis[ , list( x, y, qc = q ) ], by = c( 'x' ), 
                        all.x = TRUE, allow.cartesian = TRUE )

pob_proy_cony[ is.na( qc ), qc := 0 ]
pob_proy_cony <- pob_proy_cony[ !is.na( y ) ]
pob_proy_cony <- pob_proy_cony[ y <= 105, list( l8 = sum( l2_cot * qc ) ), 
                                by = list( t, sexo, x = y ) ]

pob_proy <- merge( pob_proy, pob_proy_dep, by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy <- merge( pob_proy, pob_proy_cony, by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy[ is.na( l7 ), l7 := 0 ]
pob_proy[ is.na( l8 ), l8 := 0 ]

message( '\tGuardando proyección de población' )
save( lf, lm, ltf, ltm, pob_proy,
      file = paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
