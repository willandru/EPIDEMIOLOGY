library(arrow)
library("vaccineff")
library("ggplot2")
library("cowplot")
library("tidyverse")
# Especifica la ruta del archivo Parquet
ruta_archivo_parquet <- "persons_fortaleza_coronavac_cohort.parquet"
cohortdata <- read_parquet(ruta_archivo_parquet)

summary(cohortdata)

glimpse(cohortdata)
cohortdata$age <- get_age_group(
  data = cohortdata,
  col_age = "age",
  max_val = 80,
  min_val = 0,
  step = 9)

cohortdata_df <- as.data.frame(cohortdata)
summary(cohortdata_df)
