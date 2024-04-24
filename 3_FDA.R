# Load required library
library(fda)
library(dplyr)
library(ggplot2)
library(fda.usc)
library(MLmetrics)
library(caret)
library(lme4)




# Set seed for reproducibility
set.seed(521)


#####################################
########## fdobj Functions ##########
#####################################

fdobj <- function(data) {
  # Peripherals
  time_points <- unique(data$Time)
  interval <- ( seq_len(max(time_points))-1 )/(max(time_points-1)) #normalize to 0-1
  
  # create 10 B-spline basis functions
  n_basis <- 10
  order <- 6
  basis_bs = create.bspline.basis(rangeval = c(0,1), nbasis = n_basis, norder = order)
  
  Mdat <- data %>%  tidyr::pivot_wider(id_cols = Time,
                                       names_from = Participant_ID, 
                                       values_from = c(HR, RR, VO2, VCO2))  %>% 
    data.matrix()
  
  Mdat <- Mdat[,-1] 
  
  # smoothed functional data
  dat_fd <- smooth.basis(interval, Mdat, basis_bs)
  
  return(dat_fd)  
}


get_fdobj <- function(data, signal) {
  # Peripherals
  time_points <- unique(data$Time)
  interval <- ( seq_len(max(time_points))-1 )/(max(time_points-1)) #normalize to 0-1
  
  # create 10 B-spline basis functions
  n_basis <- 10
  order <- 6
  basis_bs = create.bspline.basis(rangeval = c(0,1), nbasis = n_basis, norder = order)
  
  Mdat <- data %>%  tidyr::pivot_wider(id_cols = Time,
                                          names_from = Participant_ID, 
                                          values_from = signal)  %>% 
    data.matrix()
  Mdat = Mdat[,-1] 
  
  # smoothed functional data
  dat_fd <- smooth.basis(interval, Mdat, basis_bs)

  return(dat_fd)  
}




#####################################
######## Plot Smooth Curves #########
#####################################

fdobj_hr <- get_fdobj(data, "HR")
fdobj_rr <- get_fdobj(data, "RR")
fdobj_vo2 <- get_fdobj(data, "VO2")
fdobj_vco2 <- get_fdobj(data, "VCO2")

par(mfrow = c(2,2))
plot(fdobj_hr, ylab = "Registered HR")
plot(fdobj_rr, ylab = "Registered RR")
plot(fdobj_vo2, ylab = "Registered VO2")
plot(fdobj_vco2, ylab = "Registered VCO2")


#####################################











#####################################
############## t.test ###############
#####################################

## Assume grouping variable has exactly two levels
do_fun_ttest <- function(data, grouping_variable, signal) {
  levels <- data %>% dplyr::select(grouping_variable) %>% unique() %>% pull()
  data1 <- data[data[,grouping_variable] == levels[1],]
  data2 <- data[data[,grouping_variable] == levels[2],]
  
  fdobj1 <- get_fdobj(data1, signal)$fd
  fdobj2 <- get_fdobj(data2, signal)$fd
  
  
  tres <- tperm.fd(fdobj1,fdobj2, plotres=TRUE)
  
  max_q <- tres$qval
  tres_dat <-tibble(time = tres$argvals, t_values = tres$Tvals,
                    q_vals = tres$qvals.pts)
  
  p <- tres_dat %>% ggplot2::ggplot(aes(time,t_values, colour = "t_value")) + geom_line() +
    geom_line(aes(time, q_vals, colour = "q_vals")) +
    geom_line(aes(time, max_q,colour = "max_q"), linetype= "dashed") + 
    labs(x = "time", y = "") +
    ggtitle("Statistics for Pointwise t-test")
  p
  
  return(tres$pval)
}






#https://rviews.rstudio.com/2021/10/14/fda-and-the-dynamics-of-curves/
par(mfrow = c(2, 2))

do_fun_ttest(data, "Gender", "HR")
do_fun_ttest(data, "Gender", "RR")
do_fun_ttest(data, "Gender", "VO2")
do_fun_ttest(data, "Gender", "VCO2")


do_fun_ttest(data, "Puberty_Status", "HR")
do_fun_ttest(data, "Puberty_Status", "RR")
do_fun_ttest(data, "Puberty_Status", "VO2")
do_fun_ttest(data, "Puberty_Status", "VCO2")




data$Gender <- ifelse(data$Gender == "Female", 1, 0)
data$Puberty_Status <- ifelse(data$Puberty_Status == "Early", 1, 0)

gender_model <- glmer(Gender ~ Time + HR + RR git+ VO2 + VCO2 + (1 | Participant_ID), data = data, family = "binomial")
puberty_model <- glmer(Puberty_Status ~ Time + HR + RR + VO2 + VCO2 + (1 | Participant_ID), data = data, family = "binomial")

summary(gender_model)
summary(puberty_model)






