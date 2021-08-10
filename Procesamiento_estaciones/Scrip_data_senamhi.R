library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)

datos <- read.csv("qc00156212.csv", sep = "", header = F)
colnames(datos) <- c("Anio", "mes", "dia", "pp_acum", "T_max", "T_min")

datos <- dplyr::tibble(datos) %>%
  mutate(
    fecha = (paste(Anio, mes, dia, sep = "-")),
    fecha = as.Date(fecha)
  ) %>% 
  select(fecha, pp_acum, T_max, T_min) %>% 
  dplyr::arrange()

#Probando la cantidad de datos si está completo
tail(datos)
seq(as.Date("1963-08-01"), as.Date("2014-03-31"), by ="day") %>% length()
#Como podemos observar la data es completa


#ahora voy a convertir los 99.9 a NA

datos <- datos %>% 
  mutate(
    T_max = ifelse(T_max == -99.9, NA, T_max),
    T_min = ifelse(T_min ==-99.9, NA, T_min),
    pp_acum = ifelse(pp_acum == -99.9, NA, pp_acum)
  )

#Cantidad de Missing values de pp_acum, T_max, T_min


cantidad_NAs <- datos %>%
  mutate(
    pp_NA = sum(is.na(pp_acum)),
    Tmax_NA = sum(is.na(T_max)),
    Tmin_NA = sum(is.na(T_min))
    
  ) %>% 
  summarise(
    pp_NA = unique(pp_NA),
    Tmax_NA = unique(Tmax_NA),
    Tmin_NA = unique(Tmin_NA)
  )

#Calculamos la cantidad de pp_acumlada, T_max, T_min

mensual <- datos %>% 
  group_by(
    fecha = str_sub(fecha, 1, 7)
  ) %>% 
  mutate(
    pp_NAs = sum(is.na(pp_acum))*100/n(),
    Tmax_NAs = sum(is.na(T_max))*100/n(),
    Tmin_NAs = sum(is.na(T_min))*100/n()
  ) %>% 
  summarise(
    pp_acum = sum(pp_acum, na.rm = T),
    pp_NAs = unique(pp_NAs),
    T_max = mean(T_max, na.rm = T),
    Tmax_NAs = unique(Tmax_NAs),
    T_min = mean(T_min, na.rm = T),
    Tmin_NAs = unique(Tmin_NAs),
  ) %>% 
  mutate(
    pp_acum = ifelse(pp_NAs >= 10, NA, pp_acum),
    T_max = ifelse(Tmax_NAs>= 10, NA, T_max),
    T_min = ifelse(Tmin_NAs >= 10, NA, T_min),
    fecha = as.Date(sprintf("%1$s-01", fecha)),
    meses = str_sub(fecha, 6, 7)
  )

#graficamos para poder observar
ggplot(mensual) +
  geom_line(mapping = aes( x = fecha, y = pp_acum), color = "blue")



datos01 <- datos %>% 
  tidyr::separate(fecha, into = c("anio", "mes", "dia"), sep = "-") %>% 
  select(mes) %>% 
  mutate(
    mes = as.Date(mes)
  )
