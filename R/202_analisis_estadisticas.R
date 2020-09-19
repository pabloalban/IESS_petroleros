message(paste(rep("-", 100), collapse = ""))

message("\tCargando población inicial")
load(paste0(parametros$RData_seg, "IESS_pob_ini.RData"))

aux1 <- pob_ini %>% filter( sexo== 'M' )
aux1 <- as.data.frame(table(aux1$razon_social))
colnames( aux1 ) <- c('empresa',
                      'M')

aux2 <- pob_ini %>% filter( sexo== 'F' )
aux2 <- as.data.frame(table(aux2$razon_social))
colnames( aux2 ) <- c('empresa',
                      'F')

empresa_sexo <- left_join( aux1, aux2, by=c('empresa'))
empresa_sexo[which(is.na(empresa_sexo$F)),]$F <- 0

empresa_sexo['T'] <- empresa_sexo$M + empresa_sexo$F
empresa_sexo$empresa <- as.character(empresa_sexo$empresa)

empresa_sexo[9,]<-c('Total' ,sum(empresa_sexo$M), sum(empresa_sexo$F), sum(empresa_sexo$T))


empresa_sexo$empresa[9] <- 'Total'


#Distribucion por edad y sexo-----------------------------------------------------------------------
edad_sexo <- pob_ini %>%
  group_by(sexo, x) %>%
  mutate(lx=n()) %>%
  ungroup() %>%
  distinct(sexo,x, .keep_all = TRUE) %>%
  group_by(sexo) %>%
  mutate(fdp=lx/sum(lx,na.rm = TRUE)) %>%
  ungroup() %>%
  select(sexo,x, lx,fdp) %>%
  arrange(sexo,x)

#Tabla edad y sexo----------------------------------------------------------------------------------
aux <- edad_sexo %>% mutate( fdp = 100* fdp)
aux_m <- aux %>% filter( sexo == 'M') %>% select( - sexo, lx_m:= lx, fdp_m:= fdp )

aux_f <- aux %>% filter( sexo == 'F') %>% select( - sexo, lx_f:= lx, fdp_f:= fdp )


tabla_edad_sexo  <- full_join( aux_m, aux_f, by='x')
tabla_edad_sexo[which(is.na(tabla_edad_sexo$lx_f)),]$fdp_f <- 0
tabla_edad_sexo[which(is.na(tabla_edad_sexo$lx_f)),]$lx_f <- 0

tabla_edad_sexo['lx'] <- tabla_edad_sexo$lx_m + tabla_edad_sexo$lx_f

tabla_edad_sexo <- tabla_edad_sexo %>%
  mutate( fdp = 100 * lx / sum(lx, na.rm = TRUE) )

tabla_edad_sexo$x <- as.character( tabla_edad_sexo$x )

tabla_edad_sexo[71,]<-c('Total',
                        sum(tabla_edad_sexo$lx_m),
                        sum(tabla_edad_sexo$fdp_m),
                        sum(tabla_edad_sexo$lx_f),
                        sum(tabla_edad_sexo$fdp_f),
                        sum(tabla_edad_sexo$lx),
                        sum(tabla_edad_sexo$fdp)
                        )

#Guaradar en Rdata----------------------------------------------------------------------------------


message( '\tGuardando estadísticas' )
save( empresa_sexo,
      edad_sexo,
      tabla_edad_sexo,
      file = paste0( parametros$RData, 'IESS_estadisticas.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()


