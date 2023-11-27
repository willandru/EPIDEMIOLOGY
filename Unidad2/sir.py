import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt

# Parámetros del modelo SIR
beta = 0.3  # Tasa de transmisión
gamma = 0.1  # Tasa de recuperación
N = 1000  # Población total

# Condiciones iniciales
S0 = 999
I0 = 1
R0 = 0

# Definir las ecuaciones diferenciales del modelo SIR
def deriv_sir(y, t, N, beta, gamma):
    S, I, R = y
    dSdt = -beta * S * I / N
    dIdt = beta * S * I / N - gamma * I
    dRdt = gamma * I
    return dSdt, dIdt, dRdt

# Vector de condiciones iniciales
y0 = S0, I0, R0

# Vector de tiempo
t = np.linspace(0, 200, 1000)

# Resolver las ecuaciones diferenciales
solution = odeint(deriv_sir, y0, t, args=(N, beta, gamma))

# Extraer resultados
S, I, R = solution.T

# Visualización de los resultados
plt.plot(t, S, label='Susceptibles')
plt.plot(t, I, label='Infectados')
plt.plot(t, R, label='Recuperados')
plt.xlabel('Tiempo')
plt.ylabel('Población')
plt.title('Modelo SIR')
plt.legend()
plt.show()