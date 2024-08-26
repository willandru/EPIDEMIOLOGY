# Taller de evaluación de efectividad de vacunas con diseño de cohorte 
#poblacional mediante vaccineff

if(!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/vaccineff")

library(vaccineff)
library(ggplot2)

# Cargar los datos
data<-cohortdata  #from vaccineff

?cohordata # No documentation for 'cohordata'
help("cohortdata") #Not finished documentation


#Data preparation

head(cohortdata)
str(cohortdata)
unique(cohortdata$vaccine_1)
unique(cohortdata$vaccine_2)
unique(cohortdata$age)
hist(cohortdata$age)
hist(as.factor(cohortdata$sex))

# Convert 'sex' to a factor
cohortdata$sex <- factor(cohortdata$sex, levels = c("F", "M"), labels = c("Female", "Male"))

# Convert 'vaccine_1' and 'vaccine_2' to factors
cohortdata$vaccine_1 <- factor(cohortdata$vaccine_1, levels = c("BRAND1", "BRAND2"), labels = c("Brand 1", "Brand 2"))
cohortdata$vaccine_2 <- factor(cohortdata$vaccine_2, levels = c("BRAND1", "BRAND2"), labels = c("Brand 1", "Brand 2"))

sum(!is.na(cohortdata$death_date))
sum(!is.na(cohortdata$death_other_causes))
sum(!is.na(cohortdata$vaccine_date_1))
sum(!is.na(cohortdata$vaccine_date_2))
sum(!is.na(cohortdata$vaccine_1))
sum(!is.na(cohortdata$vaccine_2))

# Subconjunto para personas que recibieron ambas vacunas
vaccinated_both <- subset(cohortdata, !is.na(vaccine_date_1) & !is.na(vaccine_date_2))
# Subconjunto para personas que nunca recibieron una vacuna
never_vaccinated <- subset(cohortdata, is.na(vaccine_date_1) & is.na(vaccine_date_2))

str(vaccinated_both)
str(never_vaccinated)

#Para los vacunados
ggplot(vaccinated_both, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Edad de Personas con Ambas Vacunas",
       x = "Edad", y = "Frecuencia") +
  theme_minimal()
ggplot(vaccinated_both, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Distribución de Sexo de Personas con Ambas Vacunas",
       x = "Sexo", y = "Número de Personas") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  theme_minimal()
ggplot(vaccinated_both) +
  geom_histogram(aes(x = vaccine_date_1, fill = "Vacuna 1"), binwidth = 30, color = "black", alpha = 0.5) +
  geom_histogram(aes(x = vaccine_date_2, fill = "Vacuna 2"), binwidth = 30, color = "black", alpha = 0.5) +
  labs(title = "Fechas de Vacunación para Personas con Ambas Vacunas",
       x = "Fecha", y = "Número de Personas", fill = "Tipo de Vacuna") +
  scale_fill_manual(values = c("Vacuna 1" = "green", "Vacuna 2" = "orange")) +
  theme_minimal() +
  theme(legend.position = "top")
#Para los No vacunados
ggplot(never_vaccinated, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "black") +
  labs(title = "Distribución de Edad de Personas sin Vacunas",
       x = "Edad", y = "Frecuencia") +
  theme_minimal()
ggplot(never_vaccinated, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Distribución de Sexo de Personas sin Vacunas",
       x = "Sexo", y = "Número de Personas") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  theme_minimal()
# Combine both datasets for comparison
combined_data <- bind_rows(
  mutate(vaccinated_both, status = "Vacunados Ambos"),
  mutate(never_vaccinated, status = "Nunca Vacunados")
)

# Compare age distributions
ggplot(combined_data, aes(x = age, fill = status)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  labs(title = "Comparación de Edad entre Vacunados y No Vacunados",
       x = "Edad", y = "Frecuencia") +
  scale_fill_manual(values = c("Vacunados Ambos" = "green", "Nunca Vacunados" = "lightcoral")) +
  theme_minimal()


library(dplyr)
library(ggplot2)
library(tidyr)

# Preparar los datos para el diagrama de cintas
combined_data <- bind_rows(
  mutate(vaccinated_both, status = "Vacunados Ambos"),
  mutate(never_vaccinated, status = "Nunca Vacunados")
)

# Calcular la frecuencia acumulativa
cumulative_data <- combined_data %>%
  group_by(status, age) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(status, age) %>%
  group_by(status) %>%
  mutate(cumulative_count = cumsum(count))
ggplot(cumulative_data, aes(x = age, y = cumulative_count, fill = status)) +
  geom_ribbon(aes(ymin = 0, ymax = cumulative_count), alpha = 0.5) +
  scale_fill_manual(values = c("Vacunados Ambos" = "green", "Nunca Vacunados" = "lightcoral")) +
  labs(title = "Distribución Acumulativa de Edad",
       x = "Edad", y = "Cantidad Acumulada", fill = "Estado de Vacunación") +
  theme_minimal()


# Crear un marco de datos combinado para las fechas de muerte
death_dates <- bind_rows(
  mutate(vaccinated_both, status = "Vacunados Ambos", death_date = death_date),
  mutate(never_vaccinated, status = "Nunca Vacunados", death_date = death_date)
)
# Filtrar fechas no NA
death_dates <- filter(death_dates, !is.na(death_date))
# Calcular la frecuencia acumulativa de fechas de muerte
cumulative_deaths <- death_dates %>%
  group_by(status, death_date) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(status, death_date) %>%
  group_by(status) %>%
  mutate(cumulative_count = cumsum(count))
library(ggplot2)
ggplot(cumulative_deaths, aes(x = death_date, y = cumulative_count, fill = status)) +
  geom_ribbon(aes(ymin = 0, ymax = cumulative_count), alpha = 0.5) +
  scale_fill_manual(values = c("Vacunados Ambos" = "olivedrab3", "Nunca Vacunados" = "tomato")) +
  labs(title = "Muertes Acumulativas por Fecha",
       x = "Fecha de Muerte", y = "Cantidad Acumulada", fill = "Estado de Vacunación") +
  theme_minimal()


# Scatter plot of the dates (if relevant)
plot(cohortdata$death_date, cohortdata$death_other_causes, 
     xlab="Death Date", ylab="Death Other Causes",
     main="Scatter Plot of Death Dates")

# Correlation between the dates
cor(cohortdata$death_date, cohortdata$death_other_causes, use = "complete.obs")

hist(cohortdata$death_date, breaks = "weeks")
hist(cohortdata$death_other_causes, breaks = "weeks")

hist(vaccinated_both$death_date, breaks = "weeks")
hist(vaccinated_both$death_other_causes, breaks = "weeks")

hist(never_vaccinated$death_date, breaks = "weeks")
hist(never_vaccinated$death_other_causes, breaks = "weeks")





library(dplyr)
library(tidyr)

# Define subsets
vaccinated_both <- subset(cohortdata, !is.na(vaccine_date_1) & !is.na(vaccine_date_2))
never_vaccinated <- subset(cohortdata, is.na(vaccine_date_1) & is.na(vaccine_date_2))

# Prepare data for visualization
plot_data <- bind_rows(
  vaccinated_both %>%
    select(death_date, death_other_causes) %>%
    mutate(Status = "Vaccinated Twice"),
  never_vaccinated %>%
    select(death_date, death_other_causes) %>%
    mutate(Status = "Never Vaccinated")
) %>%
  pivot_longer(cols = c(death_date, death_other_causes), names_to = "DateType", values_to = "Date") %>%
  filter(!is.na(Date)) %>%
  count(Status, DateType)  # Count occurrences
# Create the ribbon plot
ggplot(plot_data, aes(axis1 = Status, axis2 = DateType, y = n, fill = DateType)) +
  geom_alluvium() +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  labs(title = "Distribution of Dates by Vaccination Status",
       x = "Vaccination Status",
       y = "Count",
       fill = "Date Type") +
  scale_fill_manual(values = c("death_date" = "blue", "death_other_causes" = "red"))
