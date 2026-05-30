
library(tidyverse)
library(patchwork)

dat <- read.csv2("deliver/consolidado.csv")

dat$tasa_deudores <- dat$n_deudores_hat/dat$pob_adulta_hat
dat$salario_hat_real <- dat$salario2_hat * (100/dat$ipc1989)
dat$minimo_real <- dat$minimo * (100/dat$ipc1989)
dat$tasa_consumo <- (dat$salario_hat_real - dat$minimo_real)/dat$minimo_real


theme_set(theme_bw(base_size = 15))

# SALARIOS
(dat %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=salario_hat_real, color = "Salario medio real"),linewidth = 1.2) +
  geom_line(aes(y=minimo_real, color = "Salario mínimo real"),linewidth = 1.2) +
  scale_y_continuous(labels = scales::dollar,
                     breaks = seq(0, 130000, 5000)) +
  scale_x_continuous(breaks = seq(1990, 2025, 1),
                     expand = c(0,0)) +
  
  dat %>% 
    ggplot() +
    aes(x=anio) +
    geom_line(aes(y=tasa_consumo, color = "Plus salario real"),linewidth = 1.2) +
    # geom_line(aes(y=inflacion/100, color = "Inflacion"),linewidth = 1.2) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(labels = scales::percent,
                       breaks = seq(0, 6, .2)) +
    scale_x_continuous(breaks = seq(1990, 2025, 1),
                       limits = c(1999,2025),
                       expand = c(0,0))
  ) &
  theme(legend.position = c(.4,.8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))


# INFLACIÓN
dat$infla_fitted <- NA
dat$infla_fitted <- c(fitted(lm(dat$inflacion[1:19] ~ dat$anio[1:19])),
                      fitted(lm(dat$inflacion[20:36] ~ dat$anio[20:36])))
dat %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=inflacion/100, color = "Inflacion"),linewidth = 1.2) +
  geom_line(aes(y=infla_fitted/100, color = if_else(
    anio <= 2008, "Tendencia 1990-2008", "Tendencia 2009-2025"
  )),linewidth = 1.2, linetype = "dashed", alpha = .7) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, .3, .02)) +
  scale_x_continuous(breaks = seq(1990, 2025, 2),
                     expand = c(0,0)) +
  theme(legend.position = c(.5,.8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))

# CAIDA EN PLUS SALARIO 2012-2018
(1.0615548-1.4140104)/1.4140104


# DEUDA
dat %>% 
  ggplot() +
  aes(x=anio) +
  geom_col(aes(y=tasa_deudores, fill = "% endeudamiento"),linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .02),
                     expand = c(0,0.01)) +
  scale_x_continuous(breaks = seq(1990, 2025, 1),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))

# MOROSIDAD
dat %>% 
  ggplot() +
  aes(x=anio) +
  geom_col(aes(y=cartera_vencida, fill = "% morosidad"),linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .001),
                     expand = c(0,0.001)) +
  scale_x_continuous(breaks = seq(1990, 2025, 1),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5))

mean(dat$cartera_vencida) * 100



