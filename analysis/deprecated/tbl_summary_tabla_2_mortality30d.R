tabla02 <- estudio_vacunas_t_final_selected %>%
  select(
    -c(
      cod_municipio,
      desc_municipio,
      fec_fallecimiento,
      infeccion_ingreso_dias
    )
  ) %>%
  mutate(
    age = as.numeric(age),
    estancia_hospitalaria_dias = as.numeric(estancia_hospitalaria_dias)
  ) %>%
  tbl_summary(
    by = mortality30d,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(
      age ~ 2
    )
  ) %>% 
  add_overall() %>% 
  add_p()
