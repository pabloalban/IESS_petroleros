message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_estadisticas.RData' ) )

# 2. Piramide poblacional en 2018-------------------------------------------------------------------
message( '\tGraficando piramides poblacionales' )
# 2. 1. Débitos automáticos-------------------------------------------------------------------------
aux<-as.data.table( edad_sexo ) 
max_edad<-85
min_edad<-18

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(15,85,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

