# Parámetros globales R ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tConfiguración global de R' )

options( scipen = 99 )
setNumericRounding( 2 )
options( stringsAsFactors = FALSE )

# Parámetros ---------------------------------------------------------------------------------------
message( '\tCreando entorno de parámetros' )

# Entorno con parámetros
parametros <- new.env()

# User name
parametros$user <- Sys.getenv( 'USER' )

parametros$fec_eje <- Sys.Date()

# Operating system name
parametros$opsys <- Sys.info()[[1]]

# Hostname
parametros$hostname <- Sys.info()[[4]]

#Servidor de datos
if ( parametros$hostname %in% c( 'huracan', 'tornado', 'lahar', 'empty', 'tifon','LEOVELEZ',
                                 'temu-Ubuntu', 'ava.local','DESKTOP-380U0P5', 'DESKTOP-N4VHK6P', 
                                 'HP-USER', 'LPUIOMTZ011YPH') ) {
  # global Risko
  parametros$data_server <- '/mnt/data/IESS/IESS_estudio/'
  if ( parametros$hostname %in% c('LEOVELEZ') ){ # máquina samsung
    parametros$data_server <- 'Z:/IESS/IESS_estudio/'
  }
  if ( parametros$hostname %in% c('ava.local') ){ # máquina samsung
    parametros$data_server <- '/Volumes/data/IESS/IESS_estudio/'
  }
  if ( parametros$hostname %in% c('DESKTOP-380U0P5') ){ # máquina teletrabajo
    parametros$data_server <- paste0( getwd(), '/' )
  }
  if( parametros$hostname %in% c('DESKTOP-N4VHK6P', 'LPUIOMTZ011YPH') ){
    parametros$data_server <- paste0( getwd(), '/' )
  }
  if( parametros$hostname %in% c('HP-USER') ){
    parametros$data_server <- paste0( getwd(), '/' )
  }
  } else {
  # global: se necesita acceso a la red de la direccion actuarial para conectarse
  parametros$data_server <- paste0( getwd(), '/' )
}
# local
# parametros$data_server <- paste0( getwd(), '/' )


# Directorio de trabajo
parametros$work_dir <- paste0( getwd(), '/' )

# Setting Time Zone
parametros$time_zone <- "America/Guayaquil"

# Colores IESS
parametros$iess_blue <- rgb( 0, 63, 138, maxColorValue = 255 )
parametros$iess_green <- rgb( 0, 116, 53, maxColorValue = 255 )

# Calcular balance
# parametros$calcular_balance <- FALSE

parametros$mont_prop_afi <- 0.1275

# Direcciones globables  ---------------------------------------------------------------------------
message( '\tEstableciendo directorios globales' )
parametros$empresa <- 'IESS'

message( '\tConfiguración seguro' )

# message( '\t\tLas opciones son: IVM, SAL, RTR, DES, SSC, CES' )
 parametros$seguro <- 'PE'
# if ( !( parametros$seguro %in% c( 'IVM', 'SAL', 'RTR', 'DES', 'SSC', 'CES' ) ) ) {
#   stop( 'El seguro ingresado no está entre las opciones' )
# }

# Parametro realizar análisis demográfico
  parametros$hacer_ana_dem <- FALSE
  parametros$calcular_balance <- FALSE

# Configuraciones particulares por seguro ----------------------------------------------------------
parametros$fec_fin <- ymd( '2018-12-31' )
parametros$anio_ini <- 2018
parametros$anio <- 2019 # Año del estudio
parametros$edad_max <- 105

parametros$horizonte <- 20 # en años  
parametros$fec_ini <- ymd( '2013-01-01' ) # fecha inicio del periodo de observación
parametros$reserva_ini <- 764254662.48 # reserva inicial
parametros$ana_dem <- paste0( parametros$work_dir, 'R/300_analisis_demografico.R' )
  

# Variables automáticas ----------------------------------------------------------------------------
parametros$RData <- paste0( parametros$data_server, 'RData/' )
parametros$Data <- paste0( parametros$data_server, 'Data/' )
parametros$RData_seg <- paste0( parametros$data_server, 'RData/' )
parametros$Data_seg <- paste0( parametros$data_server, 'Data/' )

parametros$reportes <- paste0( parametros$work_dir, 'Reportes/' )
parametros$resultados <- paste0( parametros$work_dir, 'Resultados/' )
parametros$reporte_seguro <- paste0( parametros$work_dir, 'Reportes/Reporte_', 
                                     parametros$seguro, '/' )

parametros$calculo_balance <- paste0( parametros$work_dir, 'R/310_calculo_escenarios_balance.R' )
parametros$reporte_genera <- paste0( parametros$work_dir, 'R/600_reporte_latex.R' )


parametros$reporte_script <- paste0( parametros$reporte_seguro, 'reporte.R' )
parametros$reporte_nombre <- paste0( parametros$empresa, '_', 
                                     parametros$seguro, '_estudio_actuarial' )
parametros$reporte_latex <- paste0( parametros$reporte_nombre, '.tex' )
parametros$resultado_seguro <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/' )
parametros$resultado_tablas <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/tablas/' )
parametros$resultado_graficos <- paste0( parametros$resultados, parametros$reporte_nombre, '_', 
                                         format( parametros$fec_eje, '%Y_%m_%d' ), '/graficos/' )

parametros$graf_modelo_1 <- 'R/401_graf_plantilla.R'
parametros$graf_ext <- '.png'

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
