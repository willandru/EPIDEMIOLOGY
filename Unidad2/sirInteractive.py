import numpy as np
from scipy.integrate import odeint
import matplotlib.pyplot as plt

def plot_sir_model(beta, gamma, N):
    # Initial conditions
    S0 = N - 1
    I0 = 1
    R0 = 0

    # Define the SIR model differential equations
    def deriv_sir(y, t, N, beta, gamma):
        S, I, R = y
        dSdt = -beta * S * I / N
        dIdt = beta * S * I / N - gamma * I
        dRdt = gamma * I
        return dSdt, dIdt, dRdt

    # Initial state vector
    y0 = S0, I0, R0

    # Time vector
    t = np.linspace(0, 200, 1000)

    # Solve the SIR model differential equations
    solution = odeint(deriv_sir, y0, t, args=(N, beta, gamma))

    # Extract results
    S, I, R = solution.T

    # Plot the results
    plt.figure(figsize=(10, 6))
    plt.plot(t, S, label='Susceptibles')
    plt.plot(t, I, label='Infectados')
    plt.plot(t, R, label='Recuperados')
    plt.xlabel('Tiempo')
    plt.ylabel('Población')
    plt.title('Modelo SIR')
    plt.legend()
    plt.show()

# Specify initial parameters
initial_beta = 0.3
initial_gamma = 0.1
initial_N = 1000

# Plot the initial SIR model
plot_sir_model(initial_beta, initial_gamma, initial_N)