#growth rate (mu): how fast pop increases per unit time during the exp phase (rate/hr)
#duplication time: tiem in hrs that takes a pop to double in number 


library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(tidyverse)


t<-read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW_filtered.tsv", 
              sep = '\t', 
              header = TRUE)
t<-t%>%
  select('Cepa', 'rep', 'temp', 'hr','OD.real')




mu <- function(df_group, roll_w = 3) {
  
  df_group <- df_group %>% 
    arrange(hr) %>% #ordered by hr
    mutate(OD.real = if_else(OD.real == 0, 1e-11, OD.real)) #replaces 0s with 1x10-11 values for log transformation 

  logOD <- log(df_group$OD.real) # log transform to make exp phase linear 
  time  <- df_group$hr
  
  #calculate instantaneous slopes between adjacent points
  slope <- diff(logOD) / diff(time)
  
  #choose the maximum smoothed slope as mu (exponential phase estimate)
  mu_est <- max(slope, na.rm = TRUE)
  
  #calculate doubling time 
  doubling_time <- log(2) / mu_est 
  tibble(mu = mu_est, t_duplicacion = doubling_time)
}




results <- t %>%
  group_by(Cepa, rep, temp) %>%
  group_modify(~ mu(.x)) %>%
  ungroup()%>%
  mutate(t_duplicacion=t_duplicacion*60) #duplication time is in minutes 


results



