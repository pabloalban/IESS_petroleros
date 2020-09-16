message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )
pob_proy <- pob_proy[ t <= parametros$horizonte ]

# Población PEA no afiliados -----------------------------------------------------------------------
message( '\tGraficando proyección PEA no afiliados' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l1 ) ]
plt_l1_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l1, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l1' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l1_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l1_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l1 ) ]
plt_l1_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l1, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l1' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l1_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l1_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población afiliados activos-----------------------------------------------------------------------
message( '\tGraficando proyección afiliados' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l2 = l2_cot ) ]
plt_l2_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l2, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l2' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l2_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l2 = l2_cot ) ]
plt_l2_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l2, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l2' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l2_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas vejez (jubilados) ---------------------------------------------------------
message( '\tGraficando proyección pensionistas por vejez' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 50, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e5 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l3 ) ]
plt_l3_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l3, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l3' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l3_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l3 ) ]
plt_l3_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l3, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l3' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l3_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Población pensionistas invalidez (jubilados por invalidez)----------------------------------------
message( '\tGraficando proyección pensionistas por invalidez' )

num_anios <- length( unique( pob_proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1e4 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'F', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l4 ) ]
plt_l4_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l4, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l4' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l4_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'M', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l4 ) ]
plt_l4_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l4, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l4' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l4_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plt_pob <- marrangeGrob( list( plt_l2_f, plt_l2_m, plt_l3_f, plt_l3_m, plt_l4_f, plt_l4_m ),
                         nrow = 2, ncol = 3, top = '' )

ggsave( plot = plt_pob, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
