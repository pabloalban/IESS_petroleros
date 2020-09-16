message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )
load( paste0( parametros$RData, 'IESS_sumarizacion_proyeccion_poblacion.RData' ) )
load( paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )


message( '\tGenerando tablas de proyección de la población' )
y_max <- parametros$anio_ini + parametros$horizonte

aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l1_f = l1, l2_f = l2, l3_f = l3, l4_f = l4, 
                                              l5_f = l5 ) ]
aux_f <- aux_f[ t <= y_max ]
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l1_m = l1, l2_m = l2, l3_m = l3, l4_m = l4, 
                                              l5_m = l5 ) ]
aux_m <- aux_m[ t <= y_max ]

# Generando tabla: iess_tab_pob_proy ---------------------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, l1_f = l1, l2_f = l2, l3_f = l3, l4_f = l4, l5_f = l5 ) ]
aux_f <- aux_f[ t <= y_max ]
aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l1_m = l1, l2_m = l2, l3_m = l3, l4_m = l4, l5_m = l5 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux<-aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla: iess_tab_pob_proy_tran ----------------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l12_f = l12, l15_f = l15, l23_f = l23, l24_f = l24, 
                                              l25_f = l25, l35_f = l35, l45_f = l45 ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini,
                                              l12_m = l12, l15_m = l15, l23_m = l23, l24_m = l24, 
                                              l25_m = l25, l35_m = l35, l45_m = l45 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[, t := as.character( t ) ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_tran.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Generando tabla: iess_tab_pob_proy_cot ------------------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, 
                                              l2_f = l2, l2_cot_f = l2_cot, l2_ces_f = l2_ces ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, 
                                              l2_m = l2, l2_cot_m = l2_cot, l2_ces_m = l2_ces ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux[ , l2 := l2_f + l2_m ]
aux[ , l2_cot := l2_cot_f + l2_cot_m ]
aux[ , l2_ces := l2_ces_f + l2_ces_m ]
aux<-aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_cot.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Población dependientes menores a 18 --------------------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, l7_men_f = l7 ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l7_men_m = l7 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_men_18.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Población cónyuges dependientes ------------------------------------------------------------------
aux_f <- pob_proy_tot_sex[ sexo == 'F', list( t = t + parametros$anio_ini, l8_f = l8 ) ]
aux_f <- aux_f[ t <= y_max ]

aux_m <- pob_proy_tot_sex[ sexo == 'M', list( t = t + parametros$anio_ini, l8_m = l8 ) ]
aux_m <- aux_m[ t <= y_max ]

aux <- merge( aux_f, aux_m, by = c( 't' ) )
aux[, t := as.character( t ) ]
aux <- aux[ t > 2018 ]
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tab_pob_proy_cony.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

# Proyecion de la PEA ------------------------------------------------------------------------------
aux <- pob_proy_tot[ , list( t = t + parametros$anio_ini, l1, l2_cot ) ]
aux <- aux[ t <= y_max ]
aux[ , pea := l1 + l2_cot ]
aux[ , pea_afiliada := l2_cot / pea ]

aux_onu<-copy( onu_pea_tot_int[ year >= parametros$anio_ini & sex == 'FM' ] )
aux_onu <- aux_onu[ year <= y_max ]
aux_onu<-aux_onu[, .( pea = sum( pea_int ) ), by = .( year )]

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
