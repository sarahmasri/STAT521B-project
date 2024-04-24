# Load required library
library(dplyr)

# Set seed for reproducibility
set.seed(521)

# Function that returns simulated data from fixed number of participants

# Define function to generate simulated time series data
generate_data <- function(n_participants) {
  # Generate gender and puberty status
  gender <- sample(c("Male", "Female"), n_participants, replace = TRUE)
  puberty_status <- sample(c("Early", "Late"), n_participants, replace = TRUE)
  
  # Generate data for each bout
  bouts <- 4
  bout_duration <- 120  # 2 minutes in seconds
  rest_duration <- 60
  total_duration <- bouts * (bout_duration+rest_duration)
  
  # Initialize data frame
  data <- data.frame(Participant_ID = rep(1:n_participants, each = total_duration),
                     Bout = rep(rep(1:bouts, each=bout_duration+rest_duration), n_participants),
                     Time = rep(1:total_duration, times = n_participants),
                     Gender = rep(gender, each = total_duration),
                     Puberty_Status = rep(puberty_status, each = total_duration),
                     Quit = rep(sample(c(0, 1), n_participants, replace = TRUE), each = total_duration),
                     HR = 0,
                     RR = 0, 
                     VO2 = 0,
                     VCO2 = 0)
  
  # Generate time series data for each participant
  for (i in 1:n_participants) {
    mu <- rnorm(1, mean = 100, sd=10)
    
    # Generate RR (no difference in functional means)
    rr <-  mu/2 + 20 * sin(8 * pi * seq(0, 1, length.out = total_duration) - 8) + 
           0.05*data[data$Participant_ID==i,]$Time +  
            rnorm(total_duration, mean = 0, sd = 15)
    
    # Generate VO2, VCO2 and HR
    if (data$Quit[i] == 0) {
      vo2 <- mu/60 +  sin(8 * pi * seq(0, 1, length.out = total_duration)- 8) + rnorm(total_duration, mean = 2, sd = 1)
      vco2 <- mu/65 + sin(8 * pi * seq(0, 1, length.out = total_duration)- 8) + rnorm(total_duration, mean = 2, sd = 1.2)
      
      hr_coeff <- rnorm(1, mean = 20, sd=5)
      hr <- mu + hr_coeff * sin(8 * pi * seq(0, 1, length.out = total_duration) - 8) + 0.05*data[data$Participant_ID==i,]$Time +  rnorm(total_duration, mean = 0, sd = 2)
    } else {
      vo2 <- mu/60 + sin(8 * pi * seq(0, 1, length.out = total_duration)- 8) + rnorm(total_duration, mean = 1.5, sd = 1)
      vco2 <- mu/65 + sin(8 * pi * seq(0, 1, length.out = total_duration)- 8) + rnorm(total_duration, mean = 1.5, sd = 1.2)
      
      hr_coeff <- rnorm(1, mean = 20, sd=5)
      hr <- mu + 25 + hr_coeff * sin(8 * pi * seq(0, 1, length.out = total_duration)- 8) + 0.05*data[data$Participant_ID==i,]$Time + rnorm(total_duration, mean = 0, sd = 2)
    }
    
    # Apply gender and puberty status differences to VO2 and VCO2
    
    if (data$Gender[i] == "Male") {
      vo2 <- vo2 + ifelse(data$Puberty_Status[i] == "Early", 50, 0)
      vco2 <- vco2 + ifelse(data$Puberty_Status[i] == "Early", 40, 0)
    } else {
      vo2 <- vo2 + ifelse(data$Puberty_Status[i] == "Early", 30, 0)
      vco2 <- vco2 + ifelse(data$Puberty_Status[i] == "Early", 20, 0)
    }
    
    # Add data to data frame
    
    data[data$Participant_ID == i,]$HR <- hr
    data[data$Participant_ID == i,]$RR <- rr
    data[data$Participant_ID == i,]$VO2 <- vo2
    data[data$Participant_ID == i,]$VCO2 <- vco2
  }
  
  return(data)
}

# Generate simulated time series data for 100 participants
data <- generate_data(100)

# Display head of the generated time series data
head(data)
