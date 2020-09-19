

message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo población inicial SGO del IESS' )

col_nom <- c( 'sexo', 'x', 'lx', 'estado' )

col_tip <- c( 'integer',
              'integer',
              'character',
              'character',
              'character',
              'integer',
              'numeric',
              'numeric' )

file <- paste0( parametros$Data, 'IESS_historial_laboral.csv' )
pob_ini <- read.csv( file, header = TRUE, sep = '\t', dec = ',', colClasses = col_tip )

pob_ini <- as_tibble( pob_ini ) %>% clean_names()

#Cargar registro civil------------------------------------------------------------------------------
load("C:/Users/AMD-PC/Documents/IESS_petroleros/Data/RCIESS.Rdata")
RCIESS <- select(as_tibble(RCIESS),
                 IDENTIFICACION,
                 GENPER,
                 FECNACPER
                 )
pob_ini <- left_join(pob_ini, RCIESS, by = c( 'cedula'='IDENTIFICACION' ) )

pob_ini <- pob_ini %>%
  clean_names() %>%
  mutate( sexo = ifelse( genper=='2',
                         'F',
                         'M')
          ) %>%
  select( -genper )

pob_ini <- pob_ini %>% 
  filter( !is.na(fecnacper) ) %>%
  mutate( x = round( age_calc( dob = fecnacper, 
                        enddate = as.Date("2020-04-30","%Y-%m-%d"), 
                        units = "years"), 0
                    )
        )

historial_laboral <- pob_ini




message( paste( rep('-', 100 ), collapse = '' ) )
#Evolución de las inversiones del Fondo de Cesantía --------------------------------------------------------------------
message( '\tLeyendo Evolución de las inversiones del Fondo de Cesantía ' )

col_nom <- c( 'anio', 'total_inver', 'rendi', 'caja', 'fondos')

col_tip <- c( 'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character' )

file <- paste0( parametros$Data_seg, 'IESS_pob_inicial.xls' )

pob_ini <-read_excel(file,
                     sheet="BasePetroleros",
                     col_names=TRUE ) %>% clean_names()

pob_ini <- pob_ini %>%
  select( -ciudad,
          -fecha_fin ) %>%
  mutate( fecha_inicio = as.character( fecha_inicio ) ) %>%
  mutate( fecha_inicio = as.Date(fecha_inicio, "%Y-%m-%d"))

pob_ini <- left_join(pob_ini, RCIESS, by = c( 'cedula_de_ciudadania'='IDENTIFICACION' ) )

pob_ini <- pob_ini %>%
  clean_names() %>%
  mutate( sexo = ifelse( genper=='2',
                         'F',
                         'M')
  ) %>%
  select( -genper )

pob_ini <- pob_ini %>% 
  filter( !is.na(fecnacper) ) %>%
  mutate( x = round( age_calc( dob = fecnacper, 
                               enddate = as.Date("2020-04-30","%Y-%m-%d"), 
                               units = "years"), 0
  )
  )



message( '\tGuardando historial laboral' )
save( historial_laboral, file = paste0( parametros$RData, 'IESS_historial_laboral.RData' ) )


message( '\tGuardando población inicial' )
save( pob_ini, file = paste0( parametros$RData, 'IESS_pob_ini.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
