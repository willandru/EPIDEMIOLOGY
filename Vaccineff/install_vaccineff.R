if ("vaccineff" %in% installed.packages()) {
  remove.packages("vaccineff")
}

if(!require("remotes")) install.packages("remotes")
remotes::install_github("epiverse-trace/vaccineff",
                        ref = "v0.0.4",
                        build_vignettes = TRUE)

library(vaccineff)
vignette("vaccineff")
