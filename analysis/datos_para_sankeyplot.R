poblacion_total_casos_covid <- 
  estudio_vacunas_t_final_selected %>% 
  nrow()


vacunados_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(pautas_covid %in% c("1", "2", "3", "4")) %>% 
  nrow()
vacunados_porcent <- 
  vacunados_recuento/poblacion_total_casos_covid
  

no_vacunados_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(pautas_covid %in% c("0")) %>% 
  nrow()
no_vacunados_porcent <- 
  no_vacunados_recuento/poblacion_total_casos_covid


vacunados_muertos_fuera_hospital_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4") 
    & hosp_stay_bin == 0
    & mortality30d == 1
  ) %>% 
  nrow()
vacunados_muertos_fuera_hospital_porcent <- 
  vacunados_muertos_fuera_hospital_recuento/




# -% Vacunados del total
# -% No Vacunados del total
-% Supervivientes que han estado vacunados
-% Supervivientes que no han estado vacunados
-% Muertos que han estado vacunados
-% Muertos que no han estado vacunados