library(tidyverse)
library(readr)
datos <- read.csv("qc00156212.csv", sep = "", header = F)
colnames(datos) <- c("Anio", "mes", "dia", "pp_acum", "T_max", "T_min")

datos <- dplyr::tibble(datos) %>%
  mutate(
    fecha = (paste(Anio, mes, dia, sep = "-"))
  ) %>% 
  select(fecha, pp_acum, T_max, T_min) %>% 
  dplyr::arrange()

#Probando la cantidad de datos si está completo
tail(datos)
seq(as.Date("1963-8-1"), as.Date("2014-3-31"), by ="day") %>% length()
#Como podemos observar la data es completa


#amora voy a convertir los 99.9 a NA

datos <- datos %>% 
  mutate(
    T_max = ifelse(T_max == -99.9, NA, T_max),
    T_min = ifelse(T_min ==-99.9, NA, T_min),
    pp_acum = ifelse(pp_acum == -99.9, NA, pp_acum)
  )

view(datos)
