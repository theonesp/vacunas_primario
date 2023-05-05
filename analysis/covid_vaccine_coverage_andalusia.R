vacc_doses <- vroom(
  "/opt/datos_compartidos/vacunas_data/total/ENC_VACUNAS_VACUNAS_TODO.txt"
) %>%
  lazy_dt() %>%
  as.data.frame()

vacc_doses2 <- vacc_doses %>% 
  mutate(FEC_VACUNACION = ymd(FEC_VACUNACION))


n_doses_per_person <- vacc_doses2 %>% count(NUHSA_ENCRIPTADO)

count_n_doses <- n_doses_per_person %>% 
  count(n)

ggplot() +
  geom_col(data = conteo_, aes(x = n, y = nn))



ddd <- vacunas_t2 %>% filter(FEC_VACUNACION > ymd("2022-03-31"))


asdasd <- vacunas_t %>% 
  select(CAMP_VACUNACION) %>% 
  mutate(dd = factor(CAMP_VACUNACION))

levels(asdasd$dd)
