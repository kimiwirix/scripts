#worldclim
#Natalia Said
#06/02/2024
#script downloads data form worldclim for future (2021-2080) projections. 
  #It delivers dataframe with temperatures from each year.
  #update:with function it can "loop" through the different models, old scirpt in basura folder. Plots deltaT depending on year and location comparing it to old data 1970-2000



library(terra)
library(geodata)
library(ggplot2)
library(reshape2)
library(dplyr)

dir.create(path = "data")

#set latitude and longitude of desired coordinates
latitude = c(35.89177,35.66645)
longitude = c(-79.0181,-78.493247)
presence<-data.frame(longitude,latitude)

#fxn que produce df con lats, longs y avg temp given the model,ssp and time range 
worldclim_fxn <- function(model, time_range, ssp) {
  
    path=paste("data/",model) #name of paths 
    forecast<-cmip6_world(model, ssp, time_range, var="bioc", res=2.5, path)
    
    latitude = c(35.89177,35.66645)
    longitude = c(-79.0181,-78.493247)
    presence<-data.frame(longitude,latitude)
    
    forecast <- terra::extract(x = forecast, y = presence[, c("longitude", "latitude")]) 
    forecast<-forecast[2]
    return (forecast) 
  }

#setting variable with past data (1970-2000) for further comparison 
bioclim_data <- worldclim_global(var = "tavg", res = 2.5, path = "data/1970")
bioclim_1970<-mean(bioclim_data, na.rm=FALSE)
bioclim_1970 <- terra::extract(x = bioclim_1970, y = presence[, c("longitude", "latitude")]) 


#models available in cmip6, model "GFDL-ESM4" crashea 
models<-c("ACCESS-CM2","ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL") 


#like a for loop that applies all the models in the vector to the fxn, https://www.statology.org/r-lapply-multiple-arguments/
forecast_2021<-data.frame(lapply(models, worldclim_fxn, time_range="2021-2040", ssp="245")); colnames(forecast_2021)<-models
forecast_2041<-data.frame(lapply(models, worldclim_fxn, time_range="2041-2060", ssp="245")); colnames(forecast_2041)<-models
forecast_2061<-data.frame(lapply(models, worldclim_fxn, time_range="2061-2080", ssp="245")); colnames(forecast_2061)<-models


#resta del future projections con el past data (1970-2000); mete dos nuevas columnas ubicación y año 
forecast_2021<-forecast_2021[,]-bioclim_1970[,2]; forecast_2021$year<-"2021-2040"; forecast_2021$ubi<-c("Mason Farm", "Central Crops Research Station")
forecast_2041<-forecast_2041[,]-bioclim_1970[,2]; forecast_2041$year<-"2041-2060"; forecast_2041$ubi<-c("Mason Farm", "Central Crops Research Station")
forecast_2061<-forecast_2061[,]-bioclim_1970[,2]; forecast_2061$year<-"2061-2080"; forecast_2061$ubi<-c("Mason Farm", "Central Crops Research Station")


#merge all dfs into one: https://dplyr.tidyverse.org/reference/bind.html
forecast_all<-bind_rows(forecast_2021,forecast_2041,forecast_2061, .id=NULL)


#boxplot: https://stackoverflow.com/questions/8510870/boxplot-from-row-values-in-a-dataframe & https://stackoverflow.com/questions/30023610/how-to-plot-2-categorical-variables-on-x-axis-and-two-continuous-variables-as-f
forecast_all <- melt(forecast_all, id.vars= c("year","ubi")) #no se que hace esto como que cambia la orientación del df
ggplot(forecast_all, aes(year, value)) + geom_boxplot() +
  facet_grid(. ~ ubi)+ #mete ubicacion como otra x-axis variable 
  ylab("deltaT") +
  xlab("Years") +
  theme(plot.caption = element_text(hjust = 0.4))+
  labs(caption = "Boxplot of the temperature difference in years 2021-2040, 2041-2060, and 2061-2080 compared to the years 1970-2000 for each location of interest: 
  Mason Farm (+35° 53′ 30.40′′, −79° 1′ 5.37′′) and Central Crops Research Station (+35° 39′ 59.22′′, −78° 29′ 35.69′′), both located in North Carolina, USA.
  Temperature values were extracted from 25 CMIP-6 models available at: https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained/")

