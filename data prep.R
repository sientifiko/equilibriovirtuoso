
library(tidyverse)
library(lubridate)
library(imputeTS)

# ================ TOTAL DEUDORES 2011 - 2025
dat1 <- readxl::read_excel("raw/SBIF_DEUD_AGIFI_NUM.xlsx", skip = 3) %>% 
  select(1, 31) %>% 
  na.omit()

dat1$Fecha <- ymd(dat1$Fecha)

dat1$mes <- month(dat1$Fecha, label = T)

dat2 <- dat1 %>% 
  filter(mes == "dic")

dat2$anio <- year(dat2$Fecha)

dat3 <- dat2 %>% 
  select(anio, n_deudores = total)

write.csv2(dat3, "prep/deudores 2011-2025.xlsx", row.names = F)


# ======== DEUDA EN MORA 90 DÍAS 2014 - 2025 ==============

dat1 <- readxl::read_excel("raw/CMF_CONT_MOR_90DMAS_CONSOL_STO_RAZ_PORC_MONT.xlsx", skip = 3)

dat1$Fecha <- ymd(dat1$Fecha)

dat1$mes <- month(dat1$Fecha, label = T)

dat2 <- dat1 %>% 
  filter(mes == "dic")

dat2$anio <- year(dat2$Fecha)

dat3 <- dat2 %>% 
  select(anio, cartera_vencida = `Índice de cartera con morosidad de 90 días o más, colocaciones totales`)

dat3$cartera_vencida <- dat3$cartera_vencida/100 

write.csv2(dat3, "prep/cartera vencida 2014-2025.xlsx", row.names = F)

# ============== CONSOLIDACIÓN FUENTES =================

maindat <- readODS::read_ods("raw/serie deuda 1990 - 2013.ods")

venc <- read.csv2("prep/cartera vencida 2014-2025.xlsx")

deud <- read.csv2("prep/deudores 2011-2025.xlsx")

est10 <- readODS::read_ods("raw/estimación 2010.ods")

temp <- deud %>% 
  left_join(venc, by = "anio")

maindat2 <- maindat %>% 
  left_join(
    temp %>% select(anio, 
                    n_deudores2 = n_deudores, 
                    cartera_vencida2 = cartera_vencida),
    by = "anio"
  ) %>% 
  mutate(n_deudores = coalesce(n_deudores, n_deudores2),
         cartera_vencida = coalesce(cartera_vencida, cartera_vencida2)) %>% 
  select(-c(n_deudores2, cartera_vencida2))

maindat3 <- maindat2 %>% 
  bind_rows(temp %>% filter(anio >= 2014))

maindat3$n_deudores_hat <- na_kalman(maindat3$n_deudores)

write.csv2(maindat3,"prep/consolidado_deuda.csv", row.names = F)

# ============== JUNTANDO SALARIO MEDIO Y SALARIO MÍNIMO ====================

dat1 <- readODS::read_ods("raw/salarios promedio.ods")

dat1 <- dat1 %>% 
  mutate(salario = case_when(
    anio == 1993 & mes %in% c("Enero", "Febrero", "Marzo") ~ NA,
    T ~ salario
  ))

dat <- dat1 %>% 
  filter(mes != "anual") %>% 
  group_by(anio) %>% 
  reframe(salario = mean(salario, na.rm = T)) %>% 
  bind_rows(dat1 %>% filter(mes == "anual")) %>% 
  select(anio, salario)
  
dat$salario2 <- dat$salario
dat$salario2[1:3] <- NA

tw <- 127389.9
temp <- c()
for (i in c(0.0385, 0.0668, 0.0716)) { # tasas jornales Reyes-Matus (2021)
  temp <- append(temp, tw * (1-i))
  tw <- tw * (1-i)
}

dat$salario2[3:1] <- temp

dat$salario_hat <- na_kalman(dat$salario)
dat$salario2_hat <- na_kalman(dat$salario2)

minsal <- read.csv("raw/salario_minimo_chile.csv") %>% 
  filter(año >= 1990,
         año != 2026) %>% 
  select(anio = año, minimo = monto_bruto)

conso <- dat %>% 
  left_join(minsal, by = "anio")

write.csv2(conso, "prep/salario medio y minimo 1990-2025", row.names = F)

# ================ LIMPIEZA INFLACIÓN =====================


dat1 <- readxl::read_excel("raw/IPC_VAR_ANUAL_HIST_NEW.xlsx")

dat1$Periodo <- ymd(dat1$Periodo)

dat1$anio <- year(dat1$Periodo)

dat2 <- dat1 %>% 
  group_by(anio) %>% 
  reframe(inflacion = mean(`1. IPC General`))

agregar_indice <- function(.data, anio, inflacion_pct, anio_base, nombre = indice) {
  .data |>
    arrange({{ anio }}, .by_group = TRUE) |>
    mutate(
      .factor = cumprod(1 + {{ inflacion_pct }} / 100),
      "{{ nombre }}" := round(
        100 * .factor / .factor[{{ anio }} == anio_base],
        2
      )
    ) |>
    select(-.factor)
}

dat3 <- dat2 %>% 
  agregar_indice(anio, inflacion, anio_base = 1989, nombre = ipc1989)


write.csv2(dat3, "prep/inflacion 1989-2026.csv", row.names = F)


















