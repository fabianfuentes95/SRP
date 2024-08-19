rm(list=ls())

# Cargar los paquetes necesarios
if (!require(pacman)) install.packages("pacman") 
library(pacman)
p_load('deSolve', 'data.table', 'ggplot2')

# modelo de crecimiento poblacional, PIB y emisiones de CO2
growth_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Endogenous auxiliary variables
    W <- gdp / population          # Riqueza per cápita
    I_E <- alpha                   # Intensidad de Emisiones (constante o función del tiempo)
    
    # Flow variables
    dP <- r_p * population                             # Crecimiento de la población
    dG <- r_g * gdp + (beta * W * population)          # Crecimiento del PIB
    dE <- I_E * gdp                                    # Emisiones de CO2
    
    list(c(dP, dG, dE))
  })
}

# Parámetros del modelo
parameters <- c(
  r_p = 0.01,   # Tasa de crecimiento poblacional
  r_g = 0.02,   # Tasa de crecimiento base del PIB
  alpha = 0.3,  # Intensidad de emisiones (constante)
  beta = 0.1    # Efecto de la riqueza per cápita en el crecimiento del PIB
)

# Condiciones iniciales
InitialConditions <- c(
  population = 1e7,  # Población inicial (~10 millones)
  gdp = 1e11,        # PIB inicial (~100 mil millones)
  emissions = 1e6    # Emisiones iniciales de CO2 (~1 millón de toneladas)
)

# Tiempos de simulación
times <- seq(
  0,  # Inicio
  50, # Fin (50 años)
  1   # Intervalo (1 año)
)

# Método de integración
intg.method <- c("rk4")

# Resolver el sistema de ecuaciones
out <- ode(y = InitialConditions,
           times = times,
           func = growth_model,
           parms = parameters,
           method = intg.method)

plot(out)

# Grafica de resultados 
ggplot(melt(as.data.frame(out), id.vars = "time"), aes(x = time, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Dynamics of Population, GDP, and CO2 Emissions",
       x = "Time (years)",
       y = "Values")  +
  theme_minimal()

# ahora modificamos los parámetros para incluir el supuesto de que el 50% de la población 
# comienza a usar autos eléctricos
 
# Modelo de crecimiento poblacional, PIB y emisiones de CO2 con adopción de EVs
growth_model_ev <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Endogenous auxiliary variables
    W <- gdp / population          # Riqueza per cápita
    EVs_fraction <- min(0.5, beta * t / max(times))  # Fracción de adopción de EVs (máximo 50%)
    I_E <- alpha * (1 - EVs_fraction * gamma)  # Intensidad de Emisiones con fuerte reducción por EVs
    
    # Flow variables
    dP <- r_p * population                             # Crecimiento de la población
    dG <- r_g * gdp + (beta * W * population)          # Crecimiento del PIB
    dE <- I_E * gdp                                    # Emisiones de CO2 ajustadas por adopción de EVs
    
    list(c(dP, dG, dE))
  })
}

# Parámetros del modelo altamente ajustados
parameters <- c(
  r_p = 0.01,   # Tasa de crecimiento poblacional
  r_g = 0.03,   # Tasa de crecimiento base del PIB
  alpha = 1.0,  # Intensidad de emisiones base (mantener alta para ver el impacto)
  beta = 0.5,   # Tasa de adopción de EVs (rápida adopción)
  gamma = 3    # Factor de impacto de los EVs en la reducción de emisiones (incrementado fuertemente)
)

# Condiciones iniciales
InitialConditions <- c(
  population = 1e7,  # Población inicial (~10 millones)
  gdp = 1e11,        # PIB inicial (~100 mil millones)
  emissions = 1e6    # Emisiones iniciales de CO2 (~1 millón de toneladas)
)

# Tiempos de simulación
times <- seq(
  0,  # Inicio
  50, # Fin (50 años)
  1   # Intervalo (1 año)
)

# Método de integración
intg.method <- c("rk4")

# Resolver el sistema de ecuaciones
out2 <- ode(y = InitialConditions,
           times = times,
           func = growth_model_ev,
           parms = parameters,
           method = intg.method)


# Grafica de resultados 
ggplot(melt(as.data.frame(out), id.vars = "time"), aes(x = time, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Dynamics of Population, GDP, and CO2 Emissions",
       x = "Time (years)",
       y = "Values")  +
  theme_minimal()

ggplot(melt(as.data.frame(out2), id.vars = "time"), aes(x = time, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Dynamics of Population, GDP, and CO2 Emissions",
       x = "Time (years)",
       y = "Values")  +
  theme_minimal()

save.image('data/electric_vehicles.RData')
