library(dplyr) 


datos <-  data.frame(edad=c(12, 20, 16, 24, 19,19,20), dosis=c(1,2,3,1,3,3,2))



# TECNICA 1: NORMAL SIN USAR TUBERÍA
datos_filtrados <- filter(datos, edad>18)
datos_con_esquema <- mutate (datos_filtrados,
                             esquema = ifelse(dosis > 2, "Completo", "Incompleto")) 
datos_agrupados <- group_by(datos_con_esquema, esquema) 
datos_por_esquema <- summarize(datos_agrupados, personas = n())


#TÉCNICA 2: TUBERIA 'PIPE'
datos_por_esquema_pipe <- filter(datos, edad>18) %>%
  mutate(esquema=ifelse(dosis>2, 'Completo', 'Incompleto')) %>%
  group_by(esquema) %>%
  summarize(personas =n())
