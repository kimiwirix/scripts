
#nuevo script apra hacer predicciones de temperaturas con worldclim 
#worldclim
#Natalia Said
#06/02/2024
#script downloads data form worldclim for future (2021-2080) projections. 
#It delivers dataframe with temperatures from each year.
#update:with function it can "loop" through the different models, old scirpt in basura folder. Plots deltaT depending on year and location comparing it to old data 1970-2000
#bioclimatic variables: https://www.worldclim.org/data/bioclim.html

library(terra)
library(geodata)
library(ggplot2)
library(reshape2)
library(dplyr)

#where models are stored
models_dir<-"C:/Users/natal/Documents/LIIGH/data/data_worldclim/modelos_worldclim/"

#set latitude and longitude of desired coordinates
#Poza Churince 26.848936419081998, -102.13452499885801
#Poza Azul 26.92412179623051, -102.12236914473262
#Las Playitas 26.91673277586228, -102.01353642758136

latitude = c(26.848936419081998,26.92412179623051,26.91673277586228)
longitude = c(-102.13452499885801,-102.12236914473262, -102.01353642758136)
presence<-data.frame(longitude,latitude)


#fxn que produce df con lats, longs y avg temp given the model,ssp and time range 
worldclim_fxn <- function(model, time_range, ssp, bioclim) {
  
  path=paste(models_dir,model,sep = '') #name of paths 
  forecast<-cmip6_world(model, ssp, time_range, var="bioc", res=2.5, path)
  
  latitude = c(26.848936419081998,26.92412179623051,26.91673277586228)
  longitude = c(-102.13452499885801,-102.12236914473262, -102.01353642758136)
  presence<-data.frame(longitude,latitude)
  
  forecast <- terra::extract(x = forecast, y = presence[, c("longitude", "latitude")]) 
  forecast<-forecast[bioclim] # #11 es el numero de la bioclimatic variable (mean temp of warmest quarter)
  return (forecast) 
}


#models available in cmip6, model "GFDL-ESM4" crashea 
models<-c("ACCESS-CM2","ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1","CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL") 
all_forecast<-data.frame()


for (bioclim_var in 2:20){ #son 19 bioclim variables pero la primera columa es el id plt 2:20
  bioclim_data <- worldclim_global(var = "bio", res = 2.5, path = paste(models_dir,"1970",sep = ''))
  bioclim_1970<- terra::extract(x=bioclim_data, y =presence[, c("longitude", "latitude")])
  bioclim_1970<-bioclim_1970[bioclim_var] #el numero es la columna que tiene la info de la bioclimatic variable (son 19 vars)
  colnames(bioclim_1970)<-"past_data"
  
  forecast_2021<-data.frame(lapply(models, worldclim_fxn, time_range="2021-2040", ssp="245", bioclim=bioclim_var)); colnames(forecast_2021)<-models
  forecast_2041<-data.frame(lapply(models, worldclim_fxn, time_range="2041-2060", ssp="245", bioclim=bioclim_var)); colnames(forecast_2041)<-models
  forecast_2061<-data.frame(lapply(models, worldclim_fxn, time_range="2061-2080", ssp="245", bioclim=bioclim_var)); colnames(forecast_2061)<-models
  
  #forecast_2021<-forecast_2021[,]-bioclim_1970[,1] #hace la resta para sacar los deltas de valores de 1970 con cada valor de cada modelo
  #forecast_2041<-forecast_2041[,]-bioclim_1970[,1] 
  #forecast_2061<-forecast_2061[,]-bioclim_1970[,1]
  
  forecast_2021$year<-"2021-2040"; forecast_2021$ubi<-c("Poza Churince", "Poza Azul", "Las Playitas")
  forecast_2041$year<-"2041-2060"; forecast_2041$ubi<-c("Poza Churince", "Poza Azul", "Las Playitas")
  forecast_2061$year<-"2061-2080"; forecast_2061$ubi<-c("Poza Churince", "Poza Azul", "Las Playitas")
  
  
  bioclim_1970$year<-"1970-2000"; bioclim_1970$ubi<-c("Poza Churince", "Poza Azul", "Las Playitas") #solo se usó cuando se agregó 1970 a la gráfica, pero si s dejan los deltas (restas) no se necesita 
  
  bound_forecast<-bind_rows(forecast_2021,forecast_2041,forecast_2061, bioclim_1970); bound_forecast$bioclim_variable<-bioclim_var-1
  #bound_forecast<-bind_rows(forecast_2021,forecast_2041,forecast_2061, .id=NULL); bound_forecast$bioclim_variable<-bioclim_var-1
  all_forecast<-rbind(all_forecast, bound_forecast)
  
}

all_forecast <- melt(all_forecast, id.vars= c("year","ubi", "bioclim_variable")) 


df_on_bioclimvar <- all_forecast%>%
  filter(bioclim_variable %in% c(5,2,1))

plot<-ggplot(df_on_bioclimvar, aes(year, value)) + geom_boxplot() +
  facet_wrap(~bioclim_variable+ubi, ncol = 3, scales = "free_y")+
  ylab("Temperature(°C)") +
  xlab("Years") 


ggsave(plot, file="C:/Users/natal/Documents/LIIGH/results/results_worldclim/plot_cuatrocienegas_bioclimvars_9.png", width = 25, height = 14, units = "cm")





