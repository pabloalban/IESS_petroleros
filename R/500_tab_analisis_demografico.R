message( paste( rep('-', 100 ), collapse = '' ) )

message( '\t Tablas del Análisis demográfico de la población cubierta' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_estadisticas.RData' ) ) 
#Función de tildes a latex--------------------------------------------------------------------------
source( 'R/502_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )
# 1. Empresa y sexo---------------------------------------------------------------------------------
aux <- empresa_sexo
aux <- tildes_a_latex(aux)
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'empresa_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

# 2. Empresa y sexo---------------------------------------------------------------------------------

tabla_edad_sexo$fdp_m<-as.numeric(tabla_edad_sexo$fdp_m)
tabla_edad_sexo$fdp_f<-as.numeric(tabla_edad_sexo$fdp_f)
tabla_edad_sexo$fdp<-as.numeric(tabla_edad_sexo$fdp)
aux <- tabla_edad_sexo
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 3, 0, 3, 0, 3) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'tabla_edad_sexo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity,
       add.to.row = list(pos = list(nrow(aux_xtable)-1),
                         command = c(paste("\\hline \n"))))

#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()