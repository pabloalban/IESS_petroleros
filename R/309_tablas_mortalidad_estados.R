message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'IESS_tabla_mortalidad_dinamica.RData' ) )

message( '\tGenerando tablas de mortalidad para cada estado' )

message( '\tMortalidad afiliados' )
afi_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, ux, qx, px ) ]
setorder( afi_mor, t, sexo, x )
afi_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
afi_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
afi_mor[ , dx := lx * qx ]
afi_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
afi_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por vejez' )
pen_vej_mor <- iess_mort_din[ x >= 50, list( t, sexo, x, ux = uvx, qx = qvx, px = pvx ) ]
setorder( pen_vej_mor, t, sexo, x )
pen_vej_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_vej_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_vej_mor[ , dx := lx * qx ]
pen_vej_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_vej_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad pensionistas por invalidez' )
pen_inv_mor <- iess_mort_din[ x >= 15, list( t, sexo, x, ux = uix, qx = qix, px = pix ) ]
setorder( pen_inv_mor, t, sexo, x )
pen_inv_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
pen_inv_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
pen_inv_mor[ , dx := lx * qx ]
pen_inv_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
pen_inv_mor[ , ex := ex / lx - 0.5 ]

message( '\tMortalidad no afiliados' )
noafi_mor <- iess_mort_din[ x >= 0, list( t, sexo, x, ux, qx = q_onu_x, px = 1 - q_onu_x ) ]
setorder( noafi_mor, t, sexo, x )
noafi_mor[ , lx := shift( px, fill = 1, type = 'lag' ), by = list( t, sexo ) ]
noafi_mor[ , lx := 1e5 * cumprod( lx ), by = list( t, sexo ) ]
noafi_mor[ , dx := lx * qx ]
noafi_mor[ , ex := rev( cumsum( rev( lx ) ) ), by = list( t, sexo ) ]
noafi_mor[ , ex := ex / lx - 0.5 ]

save( afi_mor, pen_vej_mor, pen_inv_mor, noafi_mor, 
      file = paste0( parametros$RData, 'IESS_tablas_biometricas_mortalidad_todos_estados.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
