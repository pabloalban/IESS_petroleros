# Preparación---------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Interpolación de la tasa de uso y densidad de aportes----------------------------------------------
# source( 'R/des/200_interpolacion_densidad_contribucion_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/201_interpolacion_tasa_siniestralidad_des.R', encoding = 'UTF-8', echo = FALSE )

# Calculo del balance actuarial---------------------------------------------------------------------
# source( 'R/des/310_calculo_escenarios_balance_des.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos--------------------------------------------------------------------------------
# source( 'R/402_graf_tasas_decrementos.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/403_graf_tasa_entrada.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/404_graf_macro_fin.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/405_graf_onu_life_survivors.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/407_graf_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/410_graf_poblacion_piramide.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicos Desempleo--------------------------------------------------------------------
# source( 'R/des/400_graf_estadisticas_desempleo_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/401_graf_analisis_demografico_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/402_graf_analisis_financiero_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/403_graf_inversiones_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/404_graf_balance_actuarial_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/405_graf_alisado_den_aport_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/406_graf_alisado_tasa_uso_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/407_graf_proyeccion_beneficiarios_des.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas----------------------------------------------------------------------------------
# source( 'R/500_tab_decrementos.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/501_tab_macro_fin.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/503_tab_proyeccion_poblacion.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas Desempleo----------------------------------------------------------------------
# source( 'R/des/500_tab_estadisticas_desempleo_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/501_tab_analisis_demografico_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/502_tab_analisis_contable_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/503_tab_inversiones_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/504_tab_balance_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/505_tab_escenarios_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/506_tab_primas_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/507_tab_proyeccion_poblacion_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/508_tab_estimacion_prest_des.R', encoding = 'UTF-8', echo = FALSE)
# source( 'R/des/509_tab_densidad_aportacion_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/510_tab_tasa_uso_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/512_tab_resumen_resultados_des.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/des/513_tab_afi_requisitos_des.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX-------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reporte en Excel----------------------------------------------------------------------------------
# source( 'R/des/601_reporte_balance_des.R', encoding = 'UTF-8', echo = FALSE )