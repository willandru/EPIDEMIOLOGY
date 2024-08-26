# Antes de instalar Rstan es necesario configurar C++ Toolchain. 
# Luego de haber configurado C++ Toolchain, ejecute el siguiente comando
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE) 
# 
# install.packages("dplyr")
# install.packages("incidence")
# install.packages("coarseDataTools")
# install.packages("EpiEstim")
# install.packages("ggplot2")
# install.packages("loo")
# install.packages("patchwork")
# install.packages("parallel")

# Packages on GitHub
remotes::install_github("reconhub/epicontacts")


library(dplyr)
library(epicontacts)
library(incidence)
library(coarseDataTools)
library(EpiEstim)
library(ggplot2)
library(loo)
library(patchwork)
library(rstan)




dat <- readRDS("data/practical_data_group2.RDS") 
linelist <- dat$linelist 
contacts <- dat$contacts


# Inspecionar los datos
head(linelist)


# P1
nrow(linelist)

# P2
table(linelist$sex)[2]/nrow(linelist)

summary(linelist$age)

data_edad <- linelist %>% select(age)
ggplot(data_edad, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Age") +
  xlab("Age") +
  ylab("Frequency")

# P4
table(linelist$exposure, exclude = F)[1]/nrow(linelist)


i <- incidence(linelist$date_onset)
plot(i) + 
  theme_classic() + 
  scale_fill_manual(values = "purple") +
  theme(legend.position = "none")


x <- make_epicontacts(linelist = linelist,
                      contacts = contacts,
                      from = "primary_case_id",
                      to = "secondary_case_id",
                      directed = TRUE) # Esto indica que los contactos son directos (i.e., este gráfico traza una flecha desde los casos primarios a los secundarios)

plot(x)




ip <- filter(linelist, !is.na(exposure_start) &
               !is.na(exposure_end))
nrow(ip)

ip$exposure_window <- as.numeric(ip$exposure_end - ip$exposure_start)

table(ip$exposure_window)

# Máximo tiempo de período de incubación
ip$max_ip <- ip$date_onset - ip$exposure_start
summary(as.numeric(ip$max_ip))

# Mínimo tiempo de período de incubación
ip$min_ip <- ip$date_onset - ip$exposure_end
summary(as.numeric(ip$min_ip))

# Combine the max_ip and min_ip data into a single data frame
incubation_data <- data.frame(
  type = rep(c("Máximo", "Mínimo"), each = nrow(ip)),
  value = c(as.numeric(ip$max_ip), as.numeric(ip$min_ip))
)

# Create a boxplot using ggplot2
ggplot(incubation_data, aes(x = type, y = value, fill = type)) +
  geom_boxplot() +
  ggtitle("Diagrama de Bigotes del Tiempo de Período de Incubación") +
  xlab("Tipo de Tiempo de Incubación") +
  ylab("Días") +
  theme_minimal()





# Prepare los datos
earliest_exposure <- as.Date(min(ip$exposure_start))

ip <- ip |>
  mutate(date_onset = as.numeric(date_onset - earliest_exposure),
         exposure_start = as.numeric(exposure_start - earliest_exposure),
         exposure_end = as.numeric(exposure_end - earliest_exposure)) |>
  select(id, date_onset, exposure_start, exposure_end)

# Configure algunas opciones para ejecutar las cadenas MCMC en paralelo
# Ejecución de las cadenas MCMC en paralelo significa que se ejecutaran varias cadenas al mismo tiempo usando varios núcleos de su computador
options(mc.cores=parallel::detectCores())

input_data <- list(N = length(ip$exposure_start), # NNúmero de observaciones
                   tStartExposure = ip$exposure_start,
                   tEndExposure = ip$exposure_end,
                   tSymptomOnset = ip$date_onset)

# tres distribuciones de probabilidad
distributions <- c("weibull", "gamma", "lognormal") 

# Código de Stan 
code <- sprintf("
  data{
    int<lower=1> N;
    vector[N] tStartExposure;
    vector[N] tEndExposure;
    vector[N] tSymptomOnset;
  }
  parameters{
    real<lower=0> par[2];
    vector<lower=0, upper=1>[N] uE;  // Uniform value for sampling between start and end exposure
  }
  transformed parameters{
    vector[N] tE;   // infection moment
    tE = tStartExposure + uE .* (tEndExposure - tStartExposure);
  }
model{
    // Contribution to likelihood of incubation period
    target += %s_lpdf(tSymptomOnset -  tE  | par[1], par[2]);
  }
  generated quantities {
    // likelihood for calculation of looIC
    vector[N] log_lik;
    for (i in 1:N) {
      log_lik[i] = %s_lpdf(tSymptomOnset[i] -  tE[i]  | par[1], par[2]);
    }
  }
", distributions, distributions)
names(code) <- distributions

# La siguiente línea puede tomar ~1.5 min
models <- mapply(stan_model, model_code = code)

# Toma ~40 sec.
fit <- mapply(sampling, models, list(input_data), 
              iter=3000, # Número de iteraciones (largo de la cadena MCMC)
              warmup=1000, # Número de muestras a descartar al inicio de MCMC
              chain=4) # Número de cadenas MCMC a ejecutar

pos <- mapply(function(z) rstan::extract(z)$par, fit, SIMPLIFY=FALSE) # muestreo posterior 

print(fit$gamma, digits = 3, pars = c("par[1]","par[2]")) 

rstan::traceplot(fit$gamma, pars = c("par[1]","par[2]"))

print(fit$lognormal, digits = 3, pars = c("par[1]","par[2]")) 

rstan::traceplot(fit$lognormal, pars = c("par[1]","par[2]")) 

print(fit$weibull, digits = 3, pars = c("par[1]","par[2]")) 

rstan::traceplot(fit$weibull, pars = c("par[1]","par[2]"))

# Calcule WAIC para los tres modelos
waic <- mapply(function(z) waic(extract_log_lik(z))$estimates[3,], fit)
waic



# Para looic, se necesita proveer los tamaños de muestra relativos
# al llamar a loo. Este paso lleva a mejores estimados de los tamaños de 
# muestra PSIS efectivos y del error de Monte Carlo 

# Extraer la verosimilitud puntual logarítmica para la distribución Weibull
loglik <- extract_log_lik(fit$weibull, merge_chains = FALSE)
# Obtener los tamaños de muestra relativos efectivos
r_eff <- relative_eff(exp(loglik), cores = 2)
# Calcula LOOIC
loo_w <- loo(loglik, r_eff = r_eff, cores = 2)$estimates[3,]
# Imprimir los resultados
loo_w[1]


# Extraer la verosimilitud puntual logarítmica para la distribución gamma 
loglik <- extract_log_lik(fit$gamma, merge_chains = FALSE)
r_eff <- relative_eff(exp(loglik), cores = 2)
loo_g <- loo(loglik, r_eff = r_eff, cores = 2)$estimates[3,]
loo_g[1]


# Extraer la verosimilitud puntual logarítmica para la distribución log normal 
loglik <- extract_log_lik(fit$lognormal, merge_chains = FALSE)
r_eff <- relative_eff(exp(loglik), cores = 2)
loo_l <- loo(loglik, r_eff = r_eff, cores = 2)$estimates[3,]
loo_l[1]



# Necesitamos convertir los parámetros de las distribuciones a la media y desviación estándar del rezago

# En Stan, los parámetros de las distribuciones son:
# Weibull: forma y escala
# Gamma: forma e inversa de la escala (aka rate)
# Log Normal: mu y sigma
# Referencia: https://mc-stan.org/docs/2_21/functions-reference/positive-continuous-distributions.html

# Calcule las medias
means <- cbind(
  pos$weibull[, 2] * gamma(1 + 1 / pos$weibull[, 1]), # media de Weibull
  pos$gamma[, 1] / pos$gamma[, 2], # media de gamma
  exp(pos$lognormal[, 1] + pos$lognormal[, 2]^2 / 2) # media de log normal
)

# Calcule las desviaciones estándar
standard_deviations <- cbind(
  sqrt(pos$weibull[, 2]^2 * (gamma(1 + 2 / pos$weibull[, 1]) - (gamma(1 + 1 / pos$weibull[, 1]))^2)),
  sqrt(pos$gamma[, 1] / (pos$gamma[, 2]^2)),
  sqrt((exp(pos$lognormal[, 2]^2) - 1) * (exp(2 * pos$lognormal[, 1] + pos$lognormal[, 2]^2)))
)

# Imprimir los rezagos medios e intervalos creíbles del 95%
probs <- c(0.025, 0.5, 0.975)

res_means <- apply(means, 2, quantile, probs)
colnames(res_means) <- colnames(waic) 
res_means



res_sds <- apply(standard_deviations, 2, quantile, probs)
colnames(res_sds) <- colnames(waic) 
res_sds



# Informe la mediana e intervalos creíbles del 95% para los cuantiles de cada distribución

quantiles_to_report <- c(0.025, 0.05, 0.5, 0.95, 0.975, 0.99)

# Weibull
cens_w_percentiles <- sapply(quantiles_to_report, function(p) quantile(qweibull(p = p, shape = pos$weibull[,1], scale = pos$weibull[,2]), probs = probs))
colnames(cens_w_percentiles) <- quantiles_to_report
print(cens_w_percentiles)

# Gamma
cens_g_percentiles <- sapply(quantiles_to_report, function(p) quantile(qgamma(p = p, shape = pos$gamma[,1], rate = pos$gamma[,2]), probs = probs))
colnames(cens_g_percentiles) <- quantiles_to_report
print(cens_g_percentiles)

# Log normal
cens_ln_percentiles <- sapply(quantiles_to_report, function(p) quantile(qlnorm(p = p, meanlog = pos$lognormal[,1], sdlog= pos$lognormal[,2]), probs = probs))
colnames(cens_ln_percentiles) <- quantiles_to_report
print(cens_ln_percentiles)


# Prepare los resultados para graficarlos
df <- data.frame(
  #Tome los valores de las medias para trazar la función de densidad acumulatica empirica
  inc_day = ((input_data$tSymptomOnset-input_data$tEndExposure)+(input_data$tSymptomOnset-input_data$tStartExposure))/2
)

x_plot <- seq(0, 30, by=0.1) # Esto configura el rango del eje x (número de días)

Gam_plot <- as.data.frame(list(dose= x_plot, 
                               pred= sapply(x_plot, function(q) quantile(pgamma(q = q, shape = pos$gamma[,1], rate = pos$gamma[,2]), probs = c(0.5))),
                               low = sapply(x_plot, function(q) quantile(pgamma(q = q, shape = pos$gamma[,1], rate = pos$gamma[,2]), probs = c(0.025))),
                               upp = sapply(x_plot, function(q) quantile(pgamma(q = q, shape = pos$gamma[,1], rate = pos$gamma[,2]), probs = c(0.975)))
))

Wei_plot <- as.data.frame(list(dose= x_plot, 
                               pred= sapply(x_plot, function(q) quantile(pweibull(q = q, shape = pos$weibull[,1], scale = pos$weibull[,2]), probs = c(0.5))),
                               low = sapply(x_plot, function(q) quantile(pweibull(q = q, shape = pos$weibull[,1], scale = pos$weibull[,2]), probs = c(0.025))),
                               upp = sapply(x_plot, function(q) quantile(pweibull(q = q, shape = pos$weibull[,1], scale = pos$weibull[,2]), probs = c(0.975)))
))

ln_plot <- as.data.frame(list(dose= x_plot, 
                              pred= sapply(x_plot, function(q) quantile(plnorm(q = q, meanlog = pos$lognormal[,1], sdlog= pos$lognormal[,2]), probs = c(0.5))),
                              low = sapply(x_plot, function(q) quantile(plnorm(q = q, meanlog = pos$lognormal[,1], sdlog= pos$lognormal[,2]), probs = c(0.025))),
                              upp = sapply(x_plot, function(q) quantile(plnorm(q = q, meanlog = pos$lognormal[,1], sdlog= pos$lognormal[,2]), probs = c(0.975)))
))

# Grafique las curvas de la distribución acumulada 
gamma_ggplot <- ggplot(df, aes(x=inc_day)) +
  stat_ecdf(geom = "step")+ 
  xlim(c(0, 30))+
  geom_line(data=Gam_plot, aes(x=x_plot, y=pred), color=RColorBrewer::brewer.pal(11, "RdBu")[11], linewidth=1) +
  geom_ribbon(data=Gam_plot, aes(x=x_plot,ymin=low,ymax=upp), fill = RColorBrewer::brewer.pal(11, "RdBu")[11], alpha=0.1) +
  theme_bw(base_size = 11)+
  labs(x="Incubation period (days)", y = "Proportion")+
  ggtitle("Gamma")

weibul_ggplot <- ggplot(df, aes(x=inc_day)) +
  stat_ecdf(geom = "step")+ 
  xlim(c(0, 30))+
  geom_line(data=Wei_plot, aes(x=x_plot, y=pred), color=RColorBrewer::brewer.pal(11, "RdBu")[11], linewidth=1) +
  geom_ribbon(data=Wei_plot, aes(x=x_plot,ymin=low,ymax=upp), fill = RColorBrewer::brewer.pal(11, "RdBu")[11], alpha=0.1) +
  theme_bw(base_size = 11)+
  labs(x="Incubation period (days)", y = "Proportion")+
  ggtitle("Weibull")

lognorm_ggplot <- ggplot(df, aes(x=inc_day)) +
  stat_ecdf(geom = "step")+ 
  xlim(c(0, 30))+
  geom_line(data=ln_plot, aes(x=x_plot, y=pred), color=RColorBrewer::brewer.pal(11, "RdBu")[11], linewidth=1) +
  geom_ribbon(data=ln_plot, aes(x=x_plot,ymin=low,ymax=upp), fill = RColorBrewer::brewer.pal(11, "RdBu")[11], alpha=0.1) +
  theme_bw(base_size = 11)+
  labs(x="Incubation period (days)", y = "Proportion")+
  ggtitle("Log normal")

(lognorm_ggplot|gamma_ggplot|weibul_ggplot) + plot_annotation(tag_levels = 'A') 




contacts$diff <- as.numeric(contacts$secondary_onset_date - contacts$primary_onset_date)
summary(contacts$diff)
hist(contacts$diff, xlab = "Serial interval (days)", breaks = 25, main = "", col = "pink")

