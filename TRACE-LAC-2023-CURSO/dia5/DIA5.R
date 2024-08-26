library("vaccineff")
library("ggplot2")
library("cowplot")
data(cohortdata)
head(cohortdata)

str(cohortdata)
glimpse(cohortdata)
summarise(cohortdata)


cohortdata$age_group <- get_age_group(
  data = cohortdata,
  col_age = "age",
  max_val = 80,
  min_val = 0,
  step = 9)

glimpse(cohortdata)

ggplot(data = cohortdata,
       aes(x = age_group)) +
  geom_bar() +
  theme_classic()


plot_coverage(
  data = cohortdata,
  vacc_date_col = "vaccine_date_1",
  unit = "month",
  doses_count_color = "steelblue",
  coverage_color = "mediumpurple",
  date_interval = NULL,
  cumulative = TRUE)
  
start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")
  time= c( start_cohort, end_cohort)

plot_coverage(
  data = cohortdata,
  vacc_date_col = "vaccine_date_2",
  unit = "month",
  doses_count_color = "black",
  coverage_color = "mediumpurple",
  date_interval = time,
  cumulative = FALSE)






cohortdata$immunization <-
  get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = end_cohort,
    take_first = FALSE)


cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization",
  status = c("vacc", "unvacc"))



cohortdata$death_status <- set_status(
  data = cohortdata,
  col_names = "death_date")





cohortdata$time_to_death <- get_time_to_event(
  data = cohortdata,
  outcome_date_col = "death_date",
  start_cohort = start_cohort,
  end_cohort = end_cohort,
  start_from_immunization = FALSE)





plot_survival(data = cohortdata,
              outcome_status_col = "death_status",
              time_to_event_col = "time_to_death",
              vacc_status_col = "vaccine_status",
              vaccinated_status = "vacc",
              unvaccinated_status = "unvacc",
              vaccinated_color = "steelblue",
              unvaccinated_color = "darkred",
              start_cohort = start_cohort,
              end_cohort = end_cohort,
              percentage = TRUE,
              cumulative = TRUE)


plot_survival(data = cohortdata,
              outcome_status_col = "death_status",
              time_to_event_col = "time_to_death",
              vacc_status_col = "vaccine_status",
              vaccinated_status = "vacc",
              unvaccinated_status = "unvacc",
              vaccinated_color = "steelblue",
              unvaccinated_color = "darkred",
              start_cohort = start_cohort,
              end_cohort = end_cohort,
              percentage = TRUE,
              cumulative = FALSE)
#CURVA DE SUPERVIVENCIA
PLOT <-plot_survival(data = cohortdata,
              outcome_status_col = "death_status",
              time_to_event_col = "time_to_death",
              vacc_status_col = "vaccine_status",
              vaccinated_status = "vacc",
              unvaccinated_status = "unvacc",
              vaccinated_color = "steelblue",
              unvaccinated_color = "darkred",
              start_cohort = start_cohort,
              end_cohort = end_cohort,
              percentage = TRUE,
              cumulative = FALSE)

A<- PLOT$data
cumHazard <- A$cumhaz
timeA <- A$time

plot(timeA,log(-log(cumHazard)))


coh_eff_noconf(
  data = cohortdata,
  outcome_status_col = "death_status",
  time_to_event_col = "time_to_death",
  status_vacc_col = "vaccine_status")



VACUNA1 <- cohortdata %>% group_by(age, sex) %>%
  summarise(casos = n())
ggplot(data = VACUNA1, aes(x = age, y = casos, colour = sex)) +
  geom_point()


VACUNA2 <- cohortdata %>% group_by(age, sex) %>%
  summarise(casos = n())
ggplot(data = VACUNA1, aes(x = age, y = casos, colour = sex)) +
  geom_point()