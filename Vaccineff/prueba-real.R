library("vaccineff")
data(cohortdata)
head(cohortdata)
str(cohortdata)


unique(cohortdata$sex)
unique(cohortdata$vaccine_1)
unique(cohortdata$vaccine_2)

#Esta tabla contiene información de una epidemia simulada de un virus respiratorio
#ocurrida en 2044 sobre una población hipotética, compuesta por 100.000 habitantes,
#que recibió dos dosis de una vacuna para disminuir el riesgo de muerte. 
#Sobre esta población se realizó seguimiento por 1 año, entre el 1 de enero y 31
#de diciembre de 2044.

vignette("vaccineff")
ls("package:vaccineff")
?make_vaccineff_data
?cohortdata
?effectiveness
?get_age_group
?plot_coverage


#ESTIMACION DIRECTA - SIN EMPAREJAMIENTO

# Load example data
data("cohortdata")
# Create `vaccineff_data`
vaccineff_data <- make_vaccineff_data(data_set = cohortdata,
                                      outcome_date_col = "death_date",
                                      censoring_date_col = "death_other_causes",
                                      vacc_date_col = "vaccine_date_2",
                                      vaccinated_status = "v",
                                      unvaccinated_status = "u",
                                      immunization_delay = 15,
                                      end_cohort = as.Date("2044-12-31"),
                                      match = FALSE
)
# Print summary of data
summary(vaccineff_data)

plot_coverage(vaccineff_data)

#Efectividad
VE_SIN_EMPAREJAMIENTO <- effectiveness(vaccineff_data)
summary(VE_SIN_EMPAREJAMIENTO)
plot(VE_SIN_EMPAREJAMIENTO, type = "loglog")



# Estimate the Vaccine Effectiveness at 90 days
ve90 <- effectiveness(vaccineff_data, at = 90)
# Print summary of VE
summary(ve90)
plot(ve90, type = "loglog")


# Estimate the Vaccine Effectiveness at 180 days
ve180 <- effectiveness(vaccineff_data, at = 180)
# Print summary of VE
summary(ve180)
plot(ve180, type = "loglog")



#ESTIMACION CON EMPAREJAMIENTO
# Load example data
data("cohortdata")
# Create `vaccineff_data`
vaccineff_data_EMPAREJADA <- make_vaccineff_data(data_set = cohortdata,
                                      outcome_date_col = "death_date",
                                      censoring_date_col = "death_other_causes",
                                      vacc_date_col = "vaccine_date_2",
                                      vaccinated_status = "v",
                                      unvaccinated_status = "u",
                                      immunization_delay = 15,
                                      end_cohort = as.Date("2044-12-31"),
                                      match = TRUE,
                                      exact = c("age", "sex"),
                                      nearest = NULL
)

# Print summary of data
summary(vaccineff_data)

plot_coverage(vaccineff_data)

#Efectividad
VE_SIN_EMPAREJAMIENTO <- effectiveness(vaccineff_data)
summary(VE_SIN_EMPAREJAMIENTO)
plot(VE_SIN_EMPAREJAMIENTO, type = "loglog")


# Estimate the Vaccine Effectiveness at 90 days
ve90 <- effectiveness(vaccineff_data, at = 90)
# Print summary of VE
summary(ve90)
plot(ve90, type = "loglog")


# Estimate the Vaccine Effectiveness at 180 days
ve180 <- effectiveness(vaccineff_data, at = 180)
# Print summary of VE
summary(ve180)
plot(ve180, type = "loglog")