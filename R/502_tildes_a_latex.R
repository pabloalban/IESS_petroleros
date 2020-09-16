message( '\tEstableciendo función tildes a latex' )


# Función---------- --------------------------------------------------------------------------------
#Recibe de entrada y salida objeto xtable evitando errores con al codificación UTF-8

tildes_a_latex <- function(xtb_aux) {
  
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("á", "\\\'{a}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("é", "\\\'{e}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("í", "\\\'{i}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("ó", "\\\'{o}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("ú", "\\\'{u}", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("ñ", "$\\tilde{\\text{n}}$", x, fixed = TRUE) else x }))
  xtb_aux <- xtable(xtb_aux)
  return(xtb_aux)
}
