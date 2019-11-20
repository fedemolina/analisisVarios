paquetes <- c("shiny","magrittr", "data.table", "dplyr", "ggplot2", "ggthemes", "viridis", "plotly", "hrbrthemes")
sapply(paquetes, require, character.only = TRUE)
# Que me gustaría ver en la app?

# Análisis por país
# Análisis por liga
# Análisis por clubes
# Análisis a nivel de jugadores

# Comparaciones
# Por país
# Por liga
# Por clubes
# Por jugadores

# 1. Mapa mundial con color por salario/valor de jugadores
# 
# 2. Scatter peso-height
# 
# 3. Boxplots por clubes (top) y salarios/valor de jugadores
# 
# 4. Cluster de jugadores y ver si quedan agrupados por posición
# 
# 5. Dadas las características del jugador poder inferir la posición
# 
# 6. Dada la posición inferir las características promedio


# Datos
dt <- readRDS(here::here("fifa19", "data", "fifa.rds"))
