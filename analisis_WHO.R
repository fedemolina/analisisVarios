# Análisis de datos de  World Health Organization (WHO) 

# HACER FUNCIONES GRÁFICA A REUTILIZAR.
# EJEMPLO: HACER UNA FUNCIÓN PIE CHART QUE YA GENERA UN PIE, DANDOLE LA TABLA Y LAS VARIABLES. (Si..los pie son una cagada)
paquetes = c("ggplot2", "data.table", "magrittr", "dplyr")
sapply(paquetes, require, character.only = T)

dt = data.table::fread(here::here("WHO", "who_disease.csv"))

# data.table way
dt[, ggplot(.SD, aes(x = region)) +
     geom_bar()]

dt[region == "AMR", ggplot(.SD, aes(x = year, y = cases)) +
     geom_point(alpha = 0.5)]

# pie chart
disease_counts <- dt %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(x = 1, y = total_cases, fill = disease)) +
  geom_col() + 
  coord_polar(theta = 'y')

# Data.table pie chart
dt[, .(disease_temp = fifelse(disease %in% c("measles", "mumps"), disease, "other"),
       total_cases = sum(cases)), by = disease
   ][, ggplot(.SD, aes(x = 1, y = total_cases, fill = disease_temp)) +
       geom_col() +
       coord_polar(theta = "y") +
       theme_void() +
       labs(title = "Proportion of diseases")]

