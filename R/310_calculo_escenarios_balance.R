message( '\tCalculando escenarios del balance para Desempleo' )

# Carga de hipótesis ------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )

rm( list = ls()[ !( ls() %in% c( 'parametros', 'Hipotesis' ) ) ] )

#Generando Rdatas utiles en el calculo del balance actuarial---------------------------------------
source( 'R/des/305_proyeccion_salarios_des.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/des/306_proyeccion_poblacion_des.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/des/307_estimacion_beneficio_des.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/des/308_proyeccion_beneficios_des.R', encoding = 'UTF-8', echo = FALSE )

#1. Escenario 1 -----------------------------------------------------------------------------------
esc <- new.env()

#1. 1.  Configuración del escenario 1--------------------------------------------------------------
esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

#1. 2.  Hipótesis-----------------------------------------------------------------------------------
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$V0 <- 528515073.38
esc$porcentaje_gasto <- 0

#1. 3.  Tasa de aportación de los afiliados---------------------------------------------------------
esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                           por_apo = c( rep( 0.01, 41 ) ))

#1. 4.  Factores de calibración --------------------------------------------------------------------
esc$calibra_apo <- 1.0
esc$calibra_benef <- 1 #0.839909153

#1. 5.  Calculos necesarios para el modelo actuarial------------------------------------------------
parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis','parametros' )
source( 'R/des/309_calculo_balance_des.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_DES_configuracion_', esc$nombre, '.RData' ) )
rm( esc )


#2. Escenario 2 -----------------------------------------------------------------------------------
esc <- new.env()

#2. 1.  Configuración del escenario 2--------------------------------------------------------------
esc$nombre <- 'escenario_2'
message( '\t\t\t', esc$nombre )

#2. 2.  Hipótesis-----------------------------------------------------------------------------------
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$V0 <- 528515073.38
esc$porcentaje_gasto <- 0

#2. 3.  Tasa de aportación de los afiliados---------------------------------------------------------
esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                           por_apo = c( rep( 0.01, 41 ) ))

#2. 4.  Factores de calibración --------------------------------------------------------------------
esc$calibra_apo <- 1.0
esc$calibra_benef <- 1 #0.839909153

#2. 5.  Calculos necesarios para el modelo actuarial------------------------------------------------
parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis','parametros' )
source( 'R/des/309_calculo_balance_des.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_DES_configuracion_', esc$nombre, '.RData' ) )
rm( esc )


# 3. Escenario 3 -----------------------------------------------------------------------------------
esc <- new.env()

# 3. 1.  Configuración del escenario 3--------------------------------------------------------------
esc$nombre <- 'escenario_3'
message( '\t\t\t', esc$nombre )

# 3. 2.  Hipótesis -----------------------------------------------------------------------------------
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$V0 <- 528515073.38
esc$porcentaje_gasto <- 0

#3. 3.  Tasa de aportación de los afiliados---------------------------------------------------------
esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                           por_apo = c( rep( 0.002, 41 ) ))

#3. 4.  Factores de calibración --------------------------------------------------------------------
esc$calibra_apo <- 1.0
esc$calibra_benef <- 1 #0.839909153

#3. 5.  Calculos necesarios para el modelo actuarial------------------------------------------------
parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis','parametros' )
source( 'R/des/309_calculo_balance_des.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_DES_configuracion_', esc$nombre, '.RData' ) )

#4. Cálculo de primas y análisis de ratios para todos los escenarios ---------------------------------
message( '\tCalculando primas y análisis de ratios' )
esc$gamma <- 0 # % del V0 en el calculo  prima media nivelada
source( 'R/des/311_calculo_prima_des.R', encoding = 'UTF-8', echo = FALSE )
rm( esc )

#5. Limpiando memoria RAM-----------------------------------------------------------------------------
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()