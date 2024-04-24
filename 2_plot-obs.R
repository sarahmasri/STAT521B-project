# Load required library
library(dplyr)
library("cowplot")

# Set seed for reproducibility
set.seed(521)


# Generate simulated time series data for n participants
n <- 100
data <- generate_data(n)




# Plot time series for each signal
plot_hr <-  ggplot(data[data$Participant_ID == 1, ], aes(x = Time, y = HR, group = Participant_ID, color = Bout)) +
  geom_line(alpha = 0.5) +
  labs(title = "Heart Rate (HR) Time Series",
       x = "Time (seconds)",
       y = "HR (beats/min)") +
  theme_bw()

plot_rr <- ggplot(data[data$Participant_ID == 1, ], aes(x = Time, y = RR, group = Participant_ID, color = Bout)) +
  geom_line(alpha = 0.5) +
  labs(title = "Respiratory Rate (RR) Time Series",
       x = "Time (seconds)",
       y = "RR (breaths/min)") +
  theme_bw()

plot_vo2 <- ggplot(data[data$Participant_ID == 1, ], aes(x = Time, y = VO2, group = Participant_ID, color = Bout)) +
  geom_line(alpha = 0.5) +
  labs(title = "Oxygen Uptake Rate (VO2) Time Series",
       x = "Time (seconds)",
       y = "VO2 (mL/min)") +
  theme_bw()

plot_vco2 <- ggplot(data[data$Participant_ID == 1, ], aes(x = Time, y = VCO2, group = Participant_ID, color = Bout)) +
  geom_line(alpha = 0.5) +
  labs(title = "Carbon Dioxide Uptake Rate (VCO2) Time Series",
       x = "Time (seconds)",
       y = "VCO2 (mL/min)") +
  theme_bw()

# Show data for one observation
plot_grid(plot_hr, plot_rr, plot_vo2, plot_vco2, 
          ncol = 2, nrow = 2)



