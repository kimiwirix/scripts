library(tidyverse)
library(readODS)
library(dplyr)

#There were changes in coloration in one batch made on 22.01.26, the 10 13 bottles with medium were tested on absorbance 
#in different wavelenghts.
#CONCLUSION: there is no statistical difference between the bottles based on absorbance 

data <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/MM_color.ods")

d<-data%>%
  rename('nm400'='400', 'nm420'='420', 'nm450'='450', 'nm360'='360')

d

#change nm400 for nm420, nm450, nm360
model<-lm(nm400~bottle+batch+yellow, data =d)

summary(model)
check_model(model)
