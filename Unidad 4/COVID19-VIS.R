
library(tidyverse)
library(ggplot2)

url <- "https://github.com/TRACE-LAC/TRACE-LAC-data/blob/main/otros/muestra_covid.RDS?raw=true"
datos <- readr::read_rds(url)

str(datos)
glimpse(datos)


COVID2 <- datos %>% group_by(fecha_reporte_web, sexo) 

COVID <- datos %>% group_by(fecha_reporte_web, sexo) %>%
  summarise(casos=n())

ggplot(data=COVID, aes(x=fecha_reporte_web, y=casos, colour=sexo))+
  geom_point()



COVID_FECHA <- datos %>% group_by(fecha_reporte_web) %>% 
  summarise(casos=n())


ggplot(data=COVID_FECHA, aes(x=fecha_reporte_web, y=casos))+
  geom_line()


ggplot(data=datos) + 
  geom_bar(aes(x=sexo))

COVID_DEPAS <- datos %>% group_by(nombre_departamento) %>% summarise(casos=n())

ggplot(data=COVID_DEPAS, aes(x=reorder(nombre_departamento, casos), y=casos))+
  geom_bar(stat = "identity") + coord_flip()
