
library(tidyverse)
library(imputeTS)

# ============= CONSOLIDADO FINAL ==================

deuda <- read.csv2("prep/consolidado_deuda.csv") %>% 
  select(-mes_referencia) 

salario <- read.csv2("prep/salario medio y minimo 1990-2025")

pop <- readODS::read_ods("raw/pob_adulta.ods")

ipc <- read.csv2("prep/inflacion 1989-2026.csv")

conso <- deuda %>% 
  left_join(salario, by = "anio") %>% 
  left_join(pop, by = "anio") %>% 
  mutate(pob_adulta_hat = na_kalman(pob_adulta)) %>% 
  left_join(ipc, by = "anio")

write.csv2(conso, "deliver/consolidado.csv", row.names = F)





