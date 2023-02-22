library(forestmodel)
library(skimr)
library(mgcv)
library(parallel)
library(parglm)
library(pROC)
library(ggplot2)
library(feather)
library(broom)
library(htmlTable)
library(forestmodel)
library(gtsummary)
library(tidyverse)
library(janitor)
library(stringi)
library(lubridate)

estudio_vacunas_t_final_selected <- readRDS(
  file = "/opt/datos_compartidos/vacunas_data/Rfiles/toberemoved/estudio_vacunas_t_final_selected_v13feb.rds"
) %>%
  clean_names() %>% 
  mutate(
    infect_covid_first_date = case_when(
      stri_length(as.character(infect_covid_first_date)) %in% c(8, 10) ~ infect_covid_first_date
    ),
    hosp_stay_bin = as.numeric(hosp_stay_bin),
    infeccion_ingreso_dias = as.numeric(infeccion_ingreso_dias),
    mortality30d = case_when(
      (ymd(fec_fallecimiento) - infect_covid_first_date) < 31 ~ 1,
      T ~ 0
    ),
    estancia_hospitalaria_dias = case_when(
      hosp_stay_bin == 1 ~ as.integer(estancia_hospitalaria_dias),
      T ~ NA_integer_
    )
  ) %>% 
  suppressWarnings()








# poblacion total (tenemos casos covid)
poblacion_total_casos_covid <- 
  estudio_vacunas_t_final_selected %>% 
  nrow()
poblacion_total_casos_covid



# recuento de vacunados totales
vacunados_totales_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(pautas_covid %in% c("1", "2", "3", "4")) %>% 
  nrow()
vacunados_totales_recuento
# porcentaje de vacunados totales
vacunados_totales_recuento/poblacion_total_casos_covid
# recuento de no vacunados totales
no_vacunados_totales_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(pautas_covid %in% c("0")) %>% 
  nrow()
no_vacunados_totales_recuento
# porcentaje de no vacunados totales
no_vacunados_totales_recuento/poblacion_total_casos_covid






# recuento de vacunados muertos FUERA DEL HOSPITAL
vacunados_muertos_fuera_hospital_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4") 
    & hosp_stay_bin == 0
    & mortality30d == 1
  ) %>% 
  nrow()
vacunados_muertos_fuera_hospital_recuento
# porcentaje de vacunados muertos FUERA DEL HOSPITAL
vacunados_muertos_fuera_hospital_recuento/poblacion_total_casos_covid
# recuento de no vacunados muertos FUERA DEL HOSPITAL
no_vacunados_muertos_fuera_hospital_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0") 
    & hosp_stay_bin == 0
    & mortality30d == 1
  ) %>% 
  nrow()
no_vacunados_muertos_fuera_hospital_recuento
# porcentaje de no vacunados muertos FUERA DEL HOSPITAL
no_vacunados_muertos_fuera_hospital_recuento/poblacion_total_casos_covid
# recuento de vacunados vivos FUERA DEL HOSPITAL
vacunados_vivos_fuera_hospital_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4") 
    & hosp_stay_bin == 0
    & mortality30d == 0
  ) %>% 
  nrow()
vacunados_vivos_fuera_hospital_recuento
# porcentaje de vacunados vivos FUERA DEL HOSPITAL
vacunados_vivos_fuera_hospital_recuento/poblacion_total_casos_covid
# recuento de no vacunados vivos FUERA DEL HOSPITAL
no_vacunados_vivos_fuera_hospital_recuento <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0") 
    & hosp_stay_bin == 0
    & mortality30d == 0
  ) %>% 
  nrow()
no_vacunados_vivos_fuera_hospital_recuento
# porcentaje de no vacunados vivos FUERA DEL HOSPITAL
no_vacunados_vivos_fuera_hospital_recuento/poblacion_total_casos_covid










# recuento de VACUNADOS muertos en hospital y uci
vacunados_muertos_hospitalsi_ucisi <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4") 
    & hosp_stay_bin == 1
    & icu_stay_bin == 1
    & mortality30d == 1
  ) %>% 
  nrow()
vacunados_muertos_hospitalsi_ucisi
# porcentaje de VACUNADOS muertos en hospital y uci
vacunados_muertos_hospitalsi_ucisi/poblacion_total_casos_covid
# recuento de VACUNADOS vivos en hospital y uci
vacunados_vivos_hospitalsi_ucisi <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4") 
    & hosp_stay_bin == 1
    & icu_stay_bin == 1
    & mortality30d == 0
  ) %>% 
  nrow()
vacunados_vivos_hospitalsi_ucisi
# porcentaje de VACUNADOS vivos en hospital y uci
vacunados_vivos_hospitalsi_ucisi/poblacion_total_casos_covid



# recuento de VACUNADOS muertos en hospital y SIN uci
vacunados_muertos_hospitalsi_ucino <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4") 
    & hosp_stay_bin == 1
    & icu_stay_bin == 0
    & mortality30d == 1
  ) %>% 
  nrow()
vacunados_muertos_hospitalsi_ucino
# porcentaje de VACUNADOS muertos en hospital y SIN uci
vacunados_muertos_hospitalsi_ucino/poblacion_total_casos_covid
# recuento de VACUNADOS vivos en hospital y SIN uci
vacunados_vivos_hospitalsi_ucino <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & hosp_stay_bin == 1
    & icu_stay_bin == 0
    & mortality30d == 0
  ) %>% 
  nrow()
vacunados_vivos_hospitalsi_ucino
# porcentaje de VACUNADOS vivos en hospital y SIN uci
vacunados_vivos_hospitalsi_ucino/poblacion_total_casos_covid





















# recuento de NO VACUNADOS muertos en hospital y uci
no_vacunados_muertos_hospitalsi_ucisi <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0") 
    & hosp_stay_bin == 1
    & icu_stay_bin == 1
    & mortality30d == 1
  ) %>% 
  nrow()
no_vacunados_muertos_hospitalsi_ucisi
# porcentaje de NO VACUNADOS muertos en hospital y uci
no_vacunados_muertos_hospitalsi_ucisi/poblacion_total_casos_covid
# recuento de NO VACUNADOS vivos en hospital y uci
no_vacunados_vivos_hospitalsi_ucisi <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & hosp_stay_bin == 1
    & icu_stay_bin == 1
    & mortality30d == 0
  ) %>% 
  nrow()
no_vacunados_vivos_hospitalsi_ucisi
# porcentaje de NO VACUNADOS vivos en hospital y uci
no_vacunados_vivos_hospitalsi_ucisi/poblacion_total_casos_covid



# recuento de NO VACUNADOS muertos en hospital y SIN uci
no_vacunados_muertos_hospitalsi_ucino <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0") 
    & hosp_stay_bin == 1
    & icu_stay_bin == 0
    & mortality30d == 1
  ) %>% 
  nrow()
no_vacunados_muertos_hospitalsi_ucino
# porcentaje de NO VACUNADOS muertos en hospital y SIN uci
no_vacunados_muertos_hospitalsi_ucino/poblacion_total_casos_covid
# recuento de NO VACUNADOS vivos en hospital y SIN uci
no_vacunados_vivos_hospitalsi_ucino <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & hosp_stay_bin == 1
    & icu_stay_bin == 0
    & mortality30d == 0
  ) %>% 
  nrow()
no_vacunados_vivos_hospitalsi_ucino
# porcentaje de NO VACUNADOS vivos en hospital y SIN uci
no_vacunados_vivos_hospitalsi_ucino/poblacion_total_casos_covid
















vacunados_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & hosp_stay_bin == 1)
vacunados_no_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & hosp_stay_bin == 0)
no_vacunados_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & hosp_stay_bin == 1)
no_vacunados_no_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & hosp_stay_bin == 0)



vacunados_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & icu_stay_bin == 1)
vacunados_no_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & icu_stay_bin == 0)
no_vacunados_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & icu_stay_bin == 1)
no_vacunados_no_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & icu_stay_bin == 0)



vacunados_muertos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & mortality30d == 1)
vacunados_vivos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & mortality30d == 0)
no_vacunados_muertos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & mortality30d == 1)
no_vacunados_vivos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & mortality30d == 0)
