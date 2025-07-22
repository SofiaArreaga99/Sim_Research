#Comparison for Balsam Fir 


library(ggplot2)
library(dplyr)

# === Datos ===
CF <- 1.005

# Coeficientes "originales" mean from Ry´s
B0_orig <- -2.5187
B1_orig <- 2.416

# Simulated Mean 
B0_mean <- -2.532334
B1_mean <- 2.428316


# Range of DAP for the graphic
dbh_vals <- seq(5, 80, length.out = 200)

# Create data for the two biomass lines
line_data <- data.frame(
  dbh = rep(dbh_vals, 2),
  biomasa = c((B0_orig + B1_orig * log(dbh_vals)) * CF,
              (B0_mean + B1_mean * log(dbh_vals)) * CF),
  modelo = rep(c("Original from Ry´s", "Simulated"), each = length(dbh_vals))
)


# === Graph===
ggplot() +
  # Previously simulated data
  geom_point(data = PseudoDataBalFir, aes(x = PseudoDataBalFir$dbhBalFir, y = PseudoDataBalFir$BMBalFir), alpha = 0.2, color = "gray40") +
  
  # Biomass lines
  geom_line(data = line_data, aes(x = dbh, y = biomasa, color = modelo), size = 1.2) +
  
  # Tags and theme 
  labs(
    title = "Simulation of allometric equations for Balsam Fir",
        x = "DAP (cm)",
    y = "Biomasa (kg)",
    color = "Modelo"
  ) +
  scale_color_manual(values = c("Original from Ry´s" = "red", "Simulated" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

