import numpy as np
from scipy.integrate import odeint
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

# Define the SIR model
def deriv_sir(y, t, N, beta, gamma):
    S, I, R = y
    dSdt = -beta * S * I / N
    dIdt = beta * S * I / N - gamma * I
    dRdt = gamma * I
    return dSdt, dIdt, dRdt

# Initial conditions and parameters
initial_I0 = 1
initial_R0 = 0
initial_S0 = 1000 - initial_I0 - initial_R0
initial_N = 1000
initial_beta = 0.3
initial_gamma = 0.1

# Time vector
t = np.linspace(0, 200, 1000)

# Solve the SIR model differential equations
solution = odeint(deriv_sir, [initial_S0, initial_I0, initial_R0], t, args=(initial_N, initial_beta, initial_gamma))
S, I, R = solution.T

# Create Dash app
app = dash.Dash(__name__)

# Define layout
app.layout = html.Div([
    html.H1("Interactive SIR Model"),
    dcc.Graph(id="sir-graph"),
    html.Label("Initial Susceptibles (S0)"),
    dcc.Input(id="s0-input", type="number", value=initial_S0),
    html.Label("Initial Infected (I0)"),
    dcc.Input(id="i0-input", type="number", value=initial_I0),
    html.Label("Initial Recovered (R0)"),
    dcc.Input(id="r0-input", type="number", value=initial_R0),
    html.Label("Population (N)"),
    dcc.Input(id="n-input", type="number", value=initial_N),
    html.Label("Beta"),
    dcc.Slider(id="beta-slider", min=0.1, max=1.0, step=0.1, value=initial_beta),
    html.Label("Gamma"),
    dcc.Slider(id="gamma-slider", min=0.01, max=0.5, step=0.01, value=initial_gamma)
])

# Define callback to update graph based on input values
@app.callback(
    Output("sir-graph", "figure"),
    [Input("s0-input", "value"), Input("i0-input", "value"), Input("r0-input", "value"),
     Input("n-input", "value"), Input("beta-slider", "value"), Input("gamma-slider", "value")]
)
def update_graph(s0, i0, r0, n, beta, gamma):
    # Solve the SIR model with updated parameters
    solution = odeint(deriv_sir, [s0, i0, r0], t, args=(n, beta, gamma))
    S, I, R = solution.T

    # Create the figure
    fig = make_subplots(rows=1, cols=1)
    fig.add_trace(go.Scatter(x=t, y=S, mode='lines', name='Susceptibles'))
    fig.add_trace(go.Scatter(x=t, y=I, mode='lines', name='Infectados'))
    fig.add_trace(go.Scatter(x=t, y=R, mode='lines', name='Recuperados'))

    # Update layout
    fig.update_layout(
        title="Modelo SIR",
        xaxis_title="Tiempo",
        yaxis_title="Población",
        legend=dict(x=0, y=1, traceorder='normal')
    )

    return fig

# Run the app
if __name__ == "__main__":
    app.run_server(debug=True)
