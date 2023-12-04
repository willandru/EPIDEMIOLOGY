
library(tidyverse)

# 
library(ggplot2)

url <- "https://github.com/TRACE-LAC/TRACE-LAC-data/blob/main/otros/muestra_covid.RDS?raw=true"
datos <- readr::read_rds(url)

str(datos)
glimpse(datos)
head(datos)
summary(datos)


#PARA OBSERVAR UNA SOLA VARIABLE

## Histogram of a numerical variable
ggplot(datos, aes(x = edad)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Numerical Variable")

## Bar plot for a categorical variable
ggplot(datos, aes(x = categorical_variable)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Plot of Categorical Variable")

#PARA OBSERVAR DOS VARIABLES

# Scatter plot for two numerical variables
ggplot(datos, aes(x = edad, y = codigo_divipola_municipio)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Two Numerical Variables")

# Boxplot for a numerical variable by a categorical variable
ggplot(datos, aes(x = sexo, y = codigo_divipola_municipio)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Numerical Variable by Category")

