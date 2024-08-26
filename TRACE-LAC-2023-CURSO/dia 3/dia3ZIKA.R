library(deSolve)   # Paquete deSolve para resolver las ecuaciones diferenciales
library(tidyverse) # Paquetes ggplot2 y dplyr de tidyverse
library(cowplot) # Paquete gridExtra para unir gráficos.


Lv       <- 10       # Esperanza de vida de los mosquitos (en días)
Lh       <- 50*365       # Esperanza de vida de los humanos (en días)
PIh      <- 7 # Periodo infeccioso en humanos (en días)
PIv      <-  6      # Periodo infeccioso en vectores (en días)
PEI      <-   8.4     # Período extrínseco de incubación en mosquitos adultos (en días)
muv      <-      1/Lv  # Tasa per capita de mortalidad del vector (1/Lv)
muh      <-   1/ Lh     # Tasa per capita de mortalidad del hospedador (1/Lh)
alphav   <-   muv     # Tasa per capita de natalidad del vector
alphah   <-   muh     # Tasa per capita de natalidad del hospedador
gamma    <-   1/PIh# Tasa de recuperación en humanos (1/PIh)
delta    <-   1/PEI# Tasa extrínseca de incubación (1/PEI)
ph       <-   0.7     # Probabilidad de transmisión del vector al hospedador dada una picadura por un mosquito infeccioso a un humano susceptible
pv       <-   0.7     # Probabilidad de transmisión del hospedador al vector dada una picadura por un mosquito susceptible a un humano infeccioso
Nh       <-  100000      # Número de humanos
m        <-    2    # Densidad de mosquitos hembra por humano
Nv       <-  m*Nh      # Número de vectores (m * Nh)
R0       <-   3     # Número reproductivo básico
b        <-        sqrt((R0 * muv*(muv+delta) * (muh+gamma)) /
                            (m * ph * pv * delta)) # Tasa de picadura
betah    <- ph*b       # Coeficiente de transmisión del mosquito al humano
betav    <- pv*b       # Coeficiente de transmisión del humano al mosquito
TIME     <- 100       # Número de años que se va a simular 




arbovmodel <- function(t, x, params) {
  
  Sh <- x[1]    # Humanos susceptibles
  Ih <- x[2]    # Humanos infecciosos 
  Rh <- x[3]    # Humanos recuperados
  Sv <- x[4]    # Vectores susceptibles
  Ev <- x[5]    # Vectores expuestos
  Iv <- x[6]    # Vectores infecciosos
  
  with(as.list(params), # entorno local para evaluar derivados
       {
         # Humanos
         dSh   <-  alphah * Nh - betah * (Iv/Nh) * Sh - muh * Sh   
         dIh   <-  betah * (Iv/Nh) * Sh - (gamma + muh) * Ih
         dRh   <-  gamma * Ih  - muh * Rh
         
         # Vectores
         dSv  <-  alphav * Nv - betav * (Ih/Nh) * Sv - muv * Sv 
         dEv  <-  betav * (Ih/Nh) * Sv - (delta + muv)* Ev
         dIv  <-  delta * Ev - muv * Iv
         
         dx   <- c(dSh, dIh, dRh, dSv, dEv, dIv)
         list(dx)
       }
  )
}


# Secuencia temporal (times)
times  <- seq(1, 365 * TIME , by = 1)
# Los parámetros (parms)
params <- c(
  muv      = muv,     
  muh      = muh, 
  alphav   = alphav,
  alphah   = alphah,
  gamma    = gamma,   
  delta    = delta,   
  betav    = betav,       
  betah    = betah,   
  Nh       = Nh,      
  Nv       = Nv
)
# Condiciones iniciales del sistema (y)
xstart <- c(Sh = Nh,        # COMPLETE Y COMENTE
            Ih = 0,        # COMPLETE Y COMENTE
            Rh = 0,        # COMPLETE Y COMENTE
            Sv = Nv- 1,        # COMPLETE Y COMENTE
            Ev = 0,        # COMPLETE Y COMENTE
            Iv = 1)        # COMPLETE Y COMENTE
# Resuelva las ecuaciones
out <- as.data.frame(ode(y      = xstart,   # COMPLETE Y COMENTE
                         times  = times,   # COMPLETE Y COMENTE
                         fun    = arbovmodel,   # COMPLETE Y COMENTE
                         parms  = params))  # COMPLETE Y COMENTE


# Revise el comportamiento general del modelo para 100 años
p1h <- ggplot(data = out, aes(y = (Rh + Ih + Sh), x = years)) +
  geom_line(color = 'grey68', size = 1) +
  ggtitle('Población humana total') +
  theme_bw() + ylab('Número') + xlab('Años')
p2h <- ggplot(data = out, aes(y = Sh, x = years)) +
  geom_line(color = 'royalblue', size = 1) +
  ggtitle('Población humana susceptible') +
  theme_bw() + ylab('Número') + xlab('Años')
p3h <- ggplot(data = out, aes(y = Ih, x = years)) +
  geom_line(color = 'firebrick', size = 1) +
  ggtitle('Población humana infecciosa') +
  theme_bw() + ylab('Número') + xlab('Años')
p4h <- ggplot(data = out, aes(y = Rh, x = years)) +
  geom_line(color = 'olivedrab', size = 1) +
  ggtitle('Población humana recuperada') +
  theme_bw() + ylab('Número') + xlab('Años')
plot_grid(p1h, p2h, p3h, p4h, ncol = 2)