library(tidyverse)
library(janitor)
library(stringi)
library(lubridate)



estudio_vacunas_t_final_selected <- readRDS(file = "/opt/datos_compartidos/vacunas_data/Rfiles/estudio_vacunas_t_final_selected.rds")
nrow(estudio_vacunas_t_final_selected)
summary(estudio_vacunas_t_final_selected$pautas_covid)



vacunados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(pautas_covid %in% c("1", "2", "3", "4"))
nrow(vacunados)

no_vacunados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(pautas_covid %in% c("0"))
nrow(no_vacunados)



vacunados_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & hosp_stay_bin == 1)
nrow(vacunados_ingresados)

vacunados_no_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & hosp_stay_bin == 0)
nrow(vacunados_no_ingresados)

no_vacunados_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & hosp_stay_bin == 1)
nrow(no_vacunados_ingresados)

no_vacunados_no_ingresados <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & hosp_stay_bin == 0)
nrow(no_vacunados_no_ingresados)



vacunados_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & icu_stay_bin == 1)
nrow(vacunados_ingresados_uci)

vacunados_no_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & icu_stay_bin == 0)
nrow(vacunados_no_ingresados_uci)

no_vacunados_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & icu_stay_bin == 1)
nrow(no_vacunados_ingresados_uci)

no_vacunados_no_ingresados_uci <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & icu_stay_bin == 0)
nrow(no_vacunados_no_ingresados_uci)



vacunados_muertos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & mortality30d == 1)
nrow(vacunados_muertos)

vacunados_vivos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("1", "2", "3", "4")
    & mortality30d == 0)
nrow(vacunados_vivos)

no_vacunados_muertos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & mortality30d == 1)
nrow(no_vacunados_muertos)

no_vacunados_vivos <- 
  estudio_vacunas_t_final_selected %>% 
  filter(
    pautas_covid %in% c("0")
    & mortality30d == 0)
nrow(no_vacunados_vivos)
