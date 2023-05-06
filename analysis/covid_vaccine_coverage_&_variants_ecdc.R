library(skimr)
library(mgcv)
library(parallel)
library(parglm)
library(pROC)
library(feather)
library(broom)
library(htmlTable)
library(data.table)
library(summarytools)
library(zoo)
library(magrittr)
library(lubridate)
library(dtplyr)
library(vroom)
library(stringi)
library(parallel)
library(janitor)
library(tidyverse)



vacc_doses <- vroom("/opt/datos_compartidos/vacunas_data/total/ENC_VACUNAS_VACUNAS_TODO.txt") %>%
  lazy_dt() %>%
  as.data.frame() %>% 
  mutate(FEC_VACUNACION = ymd(FEC_VACUNACION))



andal_popul_2021 <- 8472407
andal_popul_2022 <- 8500187



last_trimesterday <- 
  tibble(
    years = c(rep(2021, 4), 2022),
    months = c(3, 6, 9, 12, 3)
  ) %>% 
  mutate(
    last_trimesterday = paste(years, "-", months),
    last_trimesterday = ym(last_trimesterday),
    last_trimesterday = ceiling_date(last_trimesterday, unit = "month") - days(1)
  ) %>% 
  pull(last_trimesterday)



n_2_or_more_doses <- function(insert_trimesterday) {
  output <- vacc_doses %>% 
    filter(FEC_VACUNACION <= insert_trimesterday) %>% 
    count(NUHSA_ENCRIPTADO) %>% 
    count(n) %>% 
    filter(n >= 2) %>% 
    sum(.$nn, na.rm = TRUE)
  return(output)
}

n_2doses_2021march <- n_2_or_more_doses(insert_trimesterday = last_trimesterday[1])
n_2doses_2021june <- n_2_or_more_doses(insert_trimesterday = last_trimesterday[2])
n_2doses_2021september <- n_2_or_more_doses(insert_trimesterday = last_trimesterday[3])
n_2doses_2021december <- n_2_or_more_doses(insert_trimesterday = last_trimesterday[4])
n_2doses_2022march <- n_2_or_more_doses(insert_trimesterday = last_trimesterday[5])

# I abandon this part here; counts exceed amount of Andalusia inhabitants. 
# Better to take a snapshot of IECA



# Now, variants: info was taken from:
# https://www.ecdc.europa.eu/en/publications-data/data-virus-variants-covid-19-eueea





