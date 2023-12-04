
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

ggplot(data=COVID_DEPAS, aes(x=reorder(nombre_departamento, -casos), y=casos))+
  geom_bar(stat = "identity") + coord_flip()

ggplot(data=COVID_DEPAS, aes(x=reorder(nombre_departamento, casos), y=casos))+
  geom_bar(stat = "identity") + coord_flip() + scale_y_log10(name='Casos confirmados COVID19 ESCALA LOG()')


#FACETAS

COVID_SEXO <- datos %>% group_by(edad, sexo) %>%
  summarise(casos=n())
ggplot(COVID_SEXO, aes(x=edad, y=casos, colour=casos)) +
  geom_point()+
  facet_wrap(~sexo)+
  scale_color_gradient2()

#THEMES

ggplot(data=COVID_SEXO, aes(x=edad, y= casos))+
  geom_point()+
  facet_wrap(~sexo)+
  theme_classic()

ggplot(data=COVID_SEXO, aes(x=edad, y= casos))+
  geom_point()+
  facet_wrap(~sexo)+
  theme_bw()

#TITULOS Y EJES

ggplot(data=COVID_SEXO, aes(x=edad, y= casos, colour=sexo))+
  geom_point()+
  facet_wrap(~sexo)+ 
  labs(
    y="Casos Diarios",
    x="Edad en años",
    title="Distribucion de casos de COVID19 en COLOMBIA"
  )



#EJECRICIO RECREAR GRAFICO

COVID_RECREAR <- datos %>% group_by(fecha_de_notificacion,sexo) %>%
  summarise(casos=n())
ggplot(COVID_RECREAR, aes(x= fecha_de_notificacion, y=casos, colour=sexo ))+
  geom_line()+
  scale_color_manual(values = c("black", "yellow"))+
  theme_bw()+
  labs(
    y="Numero de Casos",
    x="Fecha de reporte a SIVIGILA",
    title="Tendencia en el número de casos COVID-19 2020-2022"
  )
