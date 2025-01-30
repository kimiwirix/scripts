hola


install.packages("sf")
install.packages("stars")
install.packages("patchwork")
install.packages("tidygeocoder",dependencies = TRUE)
install.packages("ggmap")
install.packages('tidygeocoder')
install.packages("devtools", dependencies = TRUE)
install.packages("rnaturalearthdata")

library(sp)
library(raster)
library(ggplot2)
library(patchwork)
library(dplyr, warn.conflicts = FALSE)
library(dplyr)
library(tidygeocoder)
library(ggmap)
library(sf)
library(rnaturalearth)
library(devtools)
library(ggmap)
install.packages("stars")
library(stars)

devtools::install_github("ropensci/rnaturalearthhires")

#geodata y terra
#sacar cuenta en git y usar en github 


install.packages("geodata")
library(geodata)
library(terra)


df1 <- data.frame(x = c(-79.018, -78.493247), # long
                  y = c(35.89177,35.66645)) # lat

temp<-worldclim_global("tavg",res=10,version="2.1",path="C:/Users/natal/Documents")

points <- vect(df1,
               geom=c("x", "y"),
               crs = "EPSG:4326")
values <- extract(temp,
                  points)





# cmip6_tile(lon =79.0181,lat = 35.89177, model="ACCESS-CM2", ssp=126, time="2021-2040", 
#            var = "tmin",
#            path=tempdir())

tmin<-cmip6_tile(lon =79.0181,lat = 35.89177, model="CNRM-ESM2-1", ssp=126, time="2021-2040",
                 var = "tmin",
                 path=tempdir())
tmax<-cmip6_tile(lon =79.0181,lat = 35.89177, model="CNRM-ESM2-1", ssp=126, time="2021-2040",
                 var = "tmax",
                 path=tempdir())




cmip6_tile(lon, lat, model, ssp, time, var, path, ...)


bio10 <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", 
                     var="bioc", res=10, path=tempdir())


cc<- country_codes()
head(cc)
bio10













#FUTURE 70 AÑOS and recent 

#https://bedatablog.netlify.app/post/download-and-illustrate-current-and-projected-climate-in-r/

raster::getData(name = 'worldclim', var = 'bio', res = 10)
raster::getData(name = 'CMIP5', var = 'bio', res = 10, rcp = 45, model = 'IP', year = 50)
raster::getData(name = 'CMIP5', var = 'bio', res = 10, rcp = 45, model = 'IP', year = 70)



annual_T <- stars::read_stars(paste0( "wc10/bio1.bil"))
annual_T <- annual_T/10
annual_T_70 <- stars::read_stars(paste0("cmip5/10m/ip45bi701.tif"))
annual_T_70 <- annual_T_70/10
annual_T_50 <- stars::read_stars(paste0("cmip5/10m/ip45bi501.tif"))
annual_T_50 <- annual_T_50/10

temp_colors <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))

nbreaks <- 20
{
  par(mfrow = c(1,2))
  
  plot(annual_T, main = "Annual temperature - 1960-1990",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  
  plot(annual_T_70, main = "Annual temperature - RCP 4.5 projection for 2061-2080",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  
  plot(annual_T_50, main = "Annual temperature - RCP 4.5 projection for no se",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
}


#mapa de usa
usa <- rnaturalearth::ne_countries(country = "united states of america", returnclass = "sf")

st_precision(usa) <- 1e9 
usa <- st_union(usa)
{
  par(mar = c(0,0,0,0))
  plot(usa)
}

#agrega temps a mapa de usa
annual_T_SA <- annual_T[usa]
st_crs(annual_T_70) <- st_crs(usa)
annual_T_70_SA <- annual_T_70[usa]
st_crs(annual_T_50) <- st_crs(usa)
annual_T_50_SA <- annual_T_50[usa]

#


{
  par(mfrow = c(1, 2))
  
  #ORIGINALplot(annual_T_SA, main = "Annual temperature - 1960-1990",
  #     nbreaks = nbreaks,
  #     col = temp_colors(nbreaks - 1), axes=TRUE)
  plot(annual_T_SA, main = "Annual temperature - 1960-1990",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1), axes=TRUE)
  #point<-st_point(c(35.89177,-79.018))
  #st_coordinates(points)
  plot(main = "Annual temperature - RCP 4.5 projection for 2061-2080",
       annual_T_70_SA, nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  plot(main = "Annual temperature - RCP 4.5 projection for 2041-2060",
       annual_T_50_SA, nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  
}


#checar que tipo de plot es y ver los argumentos 













plot(kk, xlim=3, ylim=8)

points=st_as_sf(annual_T_SA,coords=c("longitud","latitude"), crs=4326)
plot(st_geometry(points), pch=16, col="red")

plot(st_as_sf(annual_T_SA,coords=c("longitud","latitude"), crs=4326))












#plots que no funcionaron 
par(mfrow = c(1, 2))
recent_T_plot <- ggplot() + 
  geom_stars(data = annual_T_SA) +
  scale_fill_gradientn(name = "Annual T [°C]",
                       colors = temp_colors(5),
                       limits = c(-7, 32),
                       na.value = "white") +
  geom_sf(data = usa, fill = NA) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("a) 1960-1990") +
  theme_void() +
  theme(legend.position = "none")


projected_T_plot <- ggplot() + 
  geom_stars(data = annual_T_70_SA) +
  scale_fill_gradientn(name = "Annual T [°C]",
                       colors = temp_colors(5),
                       limits = c(-7, 32),
                       na.value = "white") +
  geom_sf(data = usa, fill = NA) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("b) 2061-2080 (projected)") +
  theme_void() +
  theme(legend.position = "bottom")

projected_T_plot<-projected_T_plot+geom_point()
print(projected_T_plot, lims_method = "geometry_bbox",default_crs = NULL)

# projected_change_T_plot <- ggplot() + 
#   geom_stars(data = annual_T_SA["change"]) +
#   scale_fill_gradientn(name = "Change in T [°C]",
#                        colors = temp_colors(5)[3:5],
#                        limits = c(1, 5),
#                        na.value = "white") +
#   geom_sf(data = usa, fill = NA) +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   ggtitle("c) Projected change") +
#   theme_void() +
#   theme(legend.position = "bottom")


#FALTA INDICAR LLAS LATS Y LONGS EN EL MAPA 


#https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html
#para sacar las lats y longs 
some_addresses<-tibble( 
  latitude = c(35.89177,35.66645),
  longitude = c(-79.0181,-78.493247)
) #https://rdrr.io/cran/tidygeocoder/man/reverse_geocode.html

lat_longs <- some_addresses %>%
  reverse_geocode(
    lat = latitude,
    long = longitude,
    method = 'osm',
    full_results = TRUE
  )

#da mapa sin color con puntos en las coordenadas especificadas
map<-ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  theme_void()














#BACKUP








#FUTURE 70 AÑOS

#https://bedatablog.netlify.app/post/download-and-illustrate-current-and-projected-climate-in-r/


# raster::getData(name = 'CMIP5', var = 'bio', res = 10, rcp = 45, model = 'IP', year = 70)
# 
# annual_T_70 <- stars::read_stars(paste0("cmip5/10m/ip45bi701.tif"))
# annual_T_70 <- annual_T_70/10
# 
# temp_colors <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))
# 
# nbreaks <- 20
# {
#   par(mfrow = c(1,2))
#   
#   plot(annual_T_70, main = "Annual temperature - RCP 4.5 projection for 2061-2080",
#        nbreaks = nbreaks,
#        col = temp_colors(nbreaks - 1))
# }
# 
# 
# #mapa de usa
# usa <- rnaturalearth::ne_countries(country = "united states of america", returnclass = "sf")
# 
# st_precision(usa) <- 1e9 
# usa <- st_union(usa)
# {
#   par(mar = c(0,0,0,0))
#   plot(usa)
# }
# 
# #agrega temps a mapa de usa
# st_crs(annual_T_70) <- st_crs(usa)
# annual_T_70_SA <- annual_T_70[usa]
# 
# {
#   par(mfrow = c(1, 2))
#   
#   plot(main = "Annual temperature - RCP 4.5 projection for 2061-2080",
#        annual_T_70_SA, nbreaks = nbreaks,
#        col = temp_colors(nbreaks - 1))
#   
#   
# }
# 
# 
# 
# 
# 
# 
# 
# #FALTA INDICAR LLAS LATS Y LONGS EN EL MAPA 
# 
# 
# #https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html
# #para sacar las lats y longs 
# some_addresses<-tibble( 
#   latitude = c(35.89177,35.66645),
#   longitude = c(-79.0181,-78.493247)
# ) #https://rdrr.io/cran/tidygeocoder/man/reverse_geocode.html
# 
# lat_longs <- some_addresses %>%
#   reverse_geocode(
#     lat = latitude,
#     long = longitude,
#     method = 'osm',
#     full_results = TRUE
#   )
# 
# #da mapa sin color con puntos en las coordenadas especificadas
# map<-ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
#   borders("state") + geom_point() +
#   ggrepel::geom_label_repel(aes(label = name)) +
#   theme_void()
# 
# 

##INTENTO 1
# 
# r <- getData("worldclim",var="bio",res=2.5)
# 
# r <- r[[c(1)]]
# names(r) <- c("Temp")
# 
# lats <- c(35.89177,35.66645)
# lons <- c(-79.0181,-78.493247)
# 
# coords <- data.frame(x=lons,y=lats)
# 
# points <- SpatialPoints(coords, proj4string = r@crs)
# 
# values <- extract(r,points)
# 
# df <- cbind.data.frame(coordinates(points),values)
# 
# values<-values/10
# 
# 
# par(bg=NA, mar=c(0,0,0,0), oma=c(0,0,0,0))
# plot (r[[1]])
# plot(points.add=T)
# head(points)
# 
# 
# #tmax_future = raster::getData('CMIP5', var='bio', res=2.5, rcp=85, model='AC', year=70, lon=-79.0181, lat=35.89177)
# 
# 
# #names(tmax_future) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# #tmax_future
# 
# 
# #recent climate data
# raster::getData(name = 'worldclim', var = 'bio', res = 10)
# 



##otro intento fallido 
target <- c("United States of America")

usa2<-ne_countries(scale = 10, returnclass = 'sf') %>%
  filter(name %in% target) %>%
  ggplot() +
  geom_sf(fill = "#e0e8a0") +
  xlim(-90,-60) +
  ylim(20,50) +
  theme(panel.background = element_rect(fill = "#8088d0"),
        panel.grid = element_line(linewidth = 0.1))

plot(usa2)











#intento numero quien sabe


dir.create(path = "recent")
dir.create(path = "año50")
dir.create(path = "año70")


library(terra)
library(geodata)


#esto si
#extracts recent data form bioclim average temp 
bioclim_data <- worldclim_global(var = "tavg",
                                 res = 2.5,
                                 path = "recent/")



#esto si
bioclim_data_2<-raster::getData(name = 'CMIP5', var = 'tmax', res = 10, rcp = 45, model = 'IP', year = 50,path="año50/")
bioclim_data_3<-raster::getData(name = 'CMIP5', var = 'tmax', res = 10, rcp = 45, model = 'IP', year = 70, path="año70/")

# bioclim_data_2 <- worldclim_global(var = "tavg",
#                                  res = 10,
#                                  rcp=45,
#                                  model="IP",
#                                  year=70,
#                                  path="año707/")
# 
# bioclim_data_3 <- worldclim_global(var = "tavg",
#                                    res = 10,
#                                    rcp=45,
#                                    model="IP",
#                                    year=50,
#                                    path = "año50/")
# 

#esto si
#lats and longs from farm and research center in NC
latitude = c(35.89177,35.66645)
longitude = c(-79.0181,-78.493247)

#esto si
#to expand the map 
max_lat<-latitude[1]+(latitude[1]*0.05)
min_lat<-latitude[2]-(latitude[2]*0.05)
max_lon<-longitude[1]-(longitude[1]*0.05)
min_lon<-longitude[2]+(longitude[2]*0.05)

#esto si
#dataframe que conecta con los 4 puntos 
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

#esto si
world_map <- world(resolution = 3,
                   path = "data/")

#esto si
# Crop the map to our area of interest
my_map <- crop(x = world_map, y = geographic_extent)

#esto si
# Plot the base map
plot(my_map,
     axes = TRUE, 
     col = "grey95")

#esto si
# Add the points for individual observations
#points(x = c(-79.0181,-78.493247), 
#       y = c(35.89177,35.66645), 
#       col = "red", 
#       pch = 20, 
#       cex = 0.75)


#esto si
# Crop bioclim data to desired extent
bioclim_data <- crop(x = bioclim_data, y = geographic_extent)

#esto no
###VER PORQUE NO FUNCIONA
bioclim_data_2 <- crop(x = bioclim_data_2, y = geographic_extent)
bioclim_data_3 <- crop(x = bioclim_data_3, y = geographic_extent)


#esto si
# Plot the first of the bioclim variables to check on cropping
plot(bioclim_data[[1]])

#esto no 
plot(bioclim_data_2[[1]])
plot(bioclim_data_3[[1]])
###ver como croppear el plot 


#esto si
points(x = c(-79.0181,-78.493247), 
       y = c(35.89177,35.66645), 
       col = "red", 
       pch = 20, 
       cex = 0.75)




#esto si
presence<-data.frame(longitude,latitude)

#esto si
# Add column indicating presence
presence$pa <- 1
head(presence)

#esto si
bioclim_extract <- raster::extract(x = bioclim_data,
                                   y = presence[, c("longitude", "latitude")],
                                   ID = FALSE) # No need for an ID column



#esto no 
bioclim_extract_2 <- raster::extract(x = bioclim_data_2,
                                     y = presence[, c("longitude", "latitude")],
                                     ID = FALSE) # No need for an ID column

bioclim_extract_3 <- extract(x = bioclim_data_3,
                             y = presence[, c("longitude", "latitude")],
                             ID = FALSE) # No need for an ID column
#esto si
points_climate <- cbind(presence, bioclim_extract)
points_climate


#esot no 
points_climate_2 <- cbind(presence, bioclim_extract_2)

points_climate_3 <- cbind(presence, bioclim_extract_3)



####despues sacar un avg entre tmin y tmax para los 70 y 50 años 



#esto no 
install.packages("tidyverse")
library(sf)
library(tidyverse)
ggplot()+
  borders("world", c("united states of america"), fill = "lightsteelblue") +
  geom_sf(data = bioclim_data_2)


kk<-c(-79.0181,35.66645,-78.493247,35.89177)
x<-c("xmin","ymin","xmax","ymax")
colnames(kk) <- x


library(ggplot2)


g<-ggplot(data=bioclim_data_2)

kk<-ext(-79.0181,35.66645,-78.493247,35.89177)

nn<-extract(bioclim_data_2, kk)
ggplot(nn)

rc<-crop(bioclim_data_2, kk)



latitude = c(35.89177,35.66645)
longitude = c(-79.0181,-78.493247)












#####################################3##

##useful
#para saber que tipo de objeto es 
#print(paste("class of x is : ",class(bioclim_data_tmax50))) 


#recent: 1970-2000 (12 files each for one month of the year)
#50: 2041-2060 
#70: 2061-2080


dir.create(path = "recent")
dir.create(path = "año50")
dir.create(path = "año70")

library(terra)
library(geodata)


#extracts recent data form bioclim average temp
bioclim_data_recent <- worldclim_global(var = "tavg",
                                        res = 2.5,
                                        path = "recent/")


#extracts projected (years 2041-2060, 2061-2080) data (tmin and tmax) from bioclim 
bioclim_data_tmin50<-raster::getData(name = 'CMIP5', var = 'tmin', res = 10, rcp = 45, model = 'IP', year = 50,path="año50/")
bioclim_data_tmax50<-raster::getData(name = 'CMIP5', var = 'tmax', res = 10, rcp = 45, model = 'IP', year = 50, path="año50/")

bioclim_data_tmin70<-raster::getData(name = 'CMIP5', var = 'tmin', res = 10, rcp = 45, model = 'IP', year = 70,path="año70/")
bioclim_data_tmax70<-raster::getData(name = 'CMIP5', var = 'tmax', res = 10, rcp = 45, model = 'IP', year = 70, path="año70/")


#lats and longs from farm and research center in NC
{
  latitude = c(35.89177,35.66645)
  longitude = c(-79.0181,-78.493247)
  
  
  #to expand the map 
  max_lat<-latitude[1]+(latitude[1]*0.05)
  min_lat<-latitude[2]-(latitude[2]*0.05)
  max_lon<-longitude[1]-(longitude[1]*0.05)
  min_lon<-longitude[2]+(longitude[2]*0.05)
  
  
  #dataframe que conecta con los 4 puntos 
  geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))
  
  
  #Download data with geodata's world function to use for our base map
  world_map <- world(resolution = 3,
                     path = "data/")
  
  
  # Crop the map to our area of interest
  my_map <- crop(x = world_map, y = geographic_extent)
  
  
  # Plot the base map
  plot(my_map,
       axes = TRUE, 
       col = "grey95")
}

#Convert rasterstack to spatraster to be able to crop it (only for CMIP5)
bioclim_data_tmax50<-as(bioclim_data_tmax50, "SpatRaster")
bioclim_data_tmin50<-as(bioclim_data_tmin50, "SpatRaster")
#unir los dos spatraster en mean 
bioclim_data_tavg50<-mean(bioclim_data_tmax50,bioclim_data_tmin50, na.rm=FALSE)


# Crop bioclim data to desired extent
bioclim_data_recent <- crop(x = bioclim_data_recent, y = geographic_extent)
bioclim_data_tavg50 <- crop(x = bioclim_data_tavg50, y = geographic_extent)

#quitar
bioclim_data_tmax50 <- crop(x = bioclim_data_tmax50, y = geographic_extent)
bioclim_data_tmin50 <- crop(x = bioclim_data_tmin50, y = geographic_extent)

#prueba
bioclim_meanspat <- crop(x = meanspat, y = geographic_extent)




#PLOTS FOR RECENT AVG TEMPS
{#https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html
  
  #sets matrix space for the 12 months
  layout(matrix(1:12, nrow=3,ncol=4, byrow=TRUE)) # Set up plot layout
  
  i=0 #counter for extracting bioclim for each month
  months=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  for (x in months){
    i=i+1
    par(mar=c(4, 4, 4, 4)) # Set margin
    
    plot(bioclim_data_recent[[i]])
    title(main=paste0("Mean ",x," Temperatures\n(1970 - 2000)"))
    place<-c("Farms","Research")
    text(longitude, latitude, labels=place, pos=2, cex=0.5)
    points(x = longitude, 
           y = latitude, 
           col = "red", 
           pch = 20, 
           cex = 1)
  }
}



#PLOTS FOR projections min&max TEMPS

#es igual una .tif por cada mes 
#buscar como hacer avg de min y max 
#plot the projected temps
#sacar un avg 
plot(bioclim_meanspat)
plot(bioclim_data_tavg50)

points(x = longitude, 
       y = latitude, 
       col = "red", 
       pch = 20, 
       cex = 1)








##DATOS EN DFs

#df de longs y lats 
presence<-data.frame(longitude,latitude)


#Extracts climate info with the coordinate sets and converts SPATRASTER to dataframe
bioclim_extract_recent <- raster::extract(x = bioclim_data_recent,
                                          y = presence[, c("longitude", "latitude")],
                                          ID = FALSE) 
bioclim_extract_tavg50 <- raster::extract(x = bioclim_data_tavg50,
                                          y = presence[, c("longitude", "latitude")],
                                          ID = FALSE) 



bioclim_extract_tmax50 <- raster::extract(x = bioclim_data_tmax50,
                                          y = presence[, c("longitude", "latitude")],
                                          ID = FALSE) 
bioclim_extract_tmin50 <- raster::extract(x = bioclim_data_tmin50,
                                          y = presence[, c("longitude", "latitude")],
                                          ID = FALSE) 



#prueba
bioclim_extract_meanspat <- raster::extract(x = bioclim_meanspat,
                                            y = presence[, c("longitude", "latitude")],
                                            ID = FALSE) 

#une tabla de lats y longs con sus datos de clima
points_climate_recent <- cbind(presence, bioclim_extract)
points_climate_tavg50 <- cbind(presence, bioclim_extract_tavg50)


points_climate_tmax50 <- cbind(presence, bioclim_extract_tmax50)
points_climate_tmin50 <- cbind(presence, bioclim_extract_tmin50)

#prueba
points_climate_meanspat <- cbind(presence, bioclim_extract_meanspat)


#une los dfs de tmin y tmax
library(data.table)
new<-rbindlist(list(points_climate_tmax50,points_climate_tmin50),use.names=FALSE)[,lapply(.SD,mean), list(longitude, latitude)]










###errores

dir.create(path = "data")
dir.create(path = "output")

bioclim_data <- worldclim_global(var = "bio",
                                 res = 2.5,
                                 path = "data/")



max_lat<-latitude[1]+(latitude[1]*0.05)
min_lat<-latitude[2]-(latitude[2]*0.05)
max_lon<-longitude[1]-(longitude[1]*0.05)
min_lon<-longitude[2]+(longitude[2]*0.05)

geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

world_map <- world(resolution = 3,
                   path = "data/")

# Crop the map to our area of interest
my_map <- crop(x = world_map, y = geographic_extent)

# Plot the base map
plot(my_map,
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observations
points(x = longitude, 
       y = latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

bioclim_data <- crop(x = bioclim_data, y = geographic_extent)
plot(bioclim_data[[1]])

presence <- data.frame(longitude, latitude)
# Add column indicating presence
presence$pa <- 1
head(presence)

bioclim_extract <- extract(x = bioclim_data,
                           y = presence[, c("longitude", "latitude")],
                           ID = FALSE) # No need for an ID column





#############################
#############################
#############################
#https://rdrr.io/github/rspatial/geodata/man/cmip6.html
#ssp 126, 245, 370
#model 
#restar future -recent y plottear la diferencia 
#tratar de necontrar datos específicos de 2010 y 2024 



##useful
#para saber que tipo de objeto es 
#print(paste("class of x is : ",class(bioclim_data_tmax50))) 


#recent: 1970-2000 (12 files each for one month of the year)
#50: 2041-2060 
#70: 2061-2080


dir.create(path = "recent")
dir.create(path = "año50")
dir.create(path = "año70")

library(terra)
library(geodata)


latitude = c(35.89177,35.66645)
longitude = c(-79.0181,-78.493247)

#extracts recent data form bioclim average temp
bioclim_data_recent <- worldclim_global(var = "tavg",
                                        res = 2.5,
                                        path = "recent/")


#2021-2040", "2041-2060", or "2061-2080"
forecast_2<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, 
                        time="2021-2040", var="bioc", res=2.5,
                        path = "2021_2040/")



#extracts projected (years 2041-2060, 2061-2080) data (tmin and tmax) from bioclim 
bioclim_data_tmin50<-raster::getData(name = 'CMIP5', var = 'tmin', res = 10, rcp = 45, model = 'IP', year = 50,path="año50/")
bioclim_data_tmax50<-raster::getData(name = 'CMIP5', var = 'tmax', res = 10, rcp = 45, model = 'IP', year = 50, path="año50/")

bioclim_data_tmin70<-raster::getData(name = 'CMIP5', var = 'tmin', res = 10, rcp = 45, model = 'IP', year = 70,path="año70/")
bioclim_data_tmax70<-raster::getData(name = 'CMIP5', var = 'tmax', res = 10, rcp = 45, model = 'IP', year = 70, path="año70/")


#lats and longs from farm and research center in NC
{
  latitude = c(35.89177,35.66645)
  longitude = c(-79.0181,-78.493247)
  
  
  #to expand the map 
  max_lat<-latitude[1]+(latitude[1]*0.05)
  min_lat<-latitude[2]-(latitude[2]*0.05)
  max_lon<-longitude[1]-(longitude[1]*0.05)
  min_lon<-longitude[2]+(longitude[2]*0.05)
  
  
  #dataframe que conecta con los 4 puntos 
  geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))
  
  
  #Download data with geodata's world function to use for our base map
  world_map <- world(resolution = 3,
                     path = "data/")
  
  
  # Crop the map to our area of interest
  my_map <- crop(x = world_map, y = geographic_extent)
  
}


#Convert rasterstack to spatraster to be able to crop it (only for CMIP5)
bioclim_data_tmax50<-as(bioclim_data_tmax50, "SpatRaster")
bioclim_data_tmin50<-as(bioclim_data_tmin50, "SpatRaster")

bioclim_data_tmax70<-as(bioclim_data_tmax70, "SpatRaster")
bioclim_data_tmin70<-as(bioclim_data_tmin70, "SpatRaster")


#unir los dos spatraster en mean 
bioclim_data_tavg50<-mean(bioclim_data_tmax50,bioclim_data_tmin50, na.rm=FALSE)
bioclim_data_tavg70<-mean(bioclim_data_tmax70,bioclim_data_tmin70, na.rm=FALSE)


# Crop bioclim data to desired extent
bioclim_data_recent <- crop(x = bioclim_data_recent, y = geographic_extent)
forecast_1<-crop(x = forecast_1, y = geographic_extent)
forecast_2<-crop(x = forecast_2, y = geographic_extent)
bioclim_data_tavg50 <- crop(x = bioclim_data_tavg50, y = geographic_extent)
bioclim_data_tavg70 <- crop(x = bioclim_data_tavg70, y = geographic_extent)



#PLOTS FOR RECENT AVG TEMPS
{#https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html
  
  #sets matrix space for the 12 months
  layout(matrix(1:12, nrow=3,ncol=4, byrow=TRUE)) # Set up plot layout
  
  i=0 #counter for extracting bioclim for each month
  months=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  for (x in months){
    i=i+1
    par(mar=c(4, 4, 4, 4)) # Set margin
    
    plot(bioclim_data_recent[[i]])
    title(main=paste0("Mean ",x," Temperatures\n(1970 - 2000)"))
    place<-c("Farms","Research")
    text(longitude, latitude, labels=place, pos=2, cex=0.75)
    points(x = longitude, 
           y = latitude, 
           col = "red", 
           pch = 20, 
           cex = 1)
  }
}


plot(forecast_1[[1]])
plot(forecast_1)
plot(forecast_2[[1]])

#PLOT FOR TAVG PROJECTED 2041-2060
{#https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html
  
  #sets matrix space for the 12 months
  layout(matrix(1:12, nrow=3,ncol=4, byrow=TRUE)) # Set up plot layout
  
  i=0 #counter for extracting bioclim for each month
  months=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  for (x in months){
    i=i+1
    par(mar=c(4, 4, 4, 4)) # Set margin
    
    plot(bioclim_data_tavg50[[i]])
    title(main=paste0("Mean ",x," Temperatures\n(2041 - 2060)"))
    place<-c("Farms","Research")
    text(longitude, latitude, labels=place, pos=2, cex=0.5)
    points(x = longitude, 
           y = latitude, 
           col = "red", 
           pch = 20, 
           cex = 1)
  }
}

#PLOT FOR TAVG PROJECTED 2061-2080
{#https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html
  
  #sets matrix space for the 12 months
  layout(matrix(1:12, nrow=3,ncol=4, byrow=TRUE)) # Set up plot layout
  
  i=0 #counter for extracting bioclim for each month
  months=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  for (x in months){
    i=i+1
    par(mar=c(4, 4, 4, 4)) # Set margin
    
    plot(bioclim_data_tavg70[[i]])
    title(main=paste0("Mean ",x," Temperatures\n(2061 - 2080)"))
    place<-c("Farms","Research")
    text(longitude, latitude, labels=place, pos=2, cex=0.5)
    points(x = longitude, 
           y = latitude, 
           col = "red", 
           pch = 20, 
           cex = 1)
  }
}


##DATOS EN DFs

#df de longs y lats 
presence<-data.frame(longitude,latitude)


#Extracts climate info with the coordinate sets and converts SPATRASTER to dataframe
{bioclim_extract_recent <- raster::extract(x = bioclim_data_recent,
                                           y = presence[, c("longitude", "latitude")],
                                           ID = FALSE) 
  bioclim_extract_tavg50 <- raster::extract(x = bioclim_data_tavg50,
                                            y = presence[, c("longitude", "latitude")],
                                            ID = FALSE) 
  bioclim_extract_tavg70 <- raster::extract(x = bioclim_data_tavg70,
                                            y = presence[, c("longitude", "latitude")],
                                            ID = FALSE)
}

bioclim_forecast <- raster::extract(x = forecast_1,
                                    y = presence[, c("longitude", "latitude")],
                                    ID = FALSE) 
bioclim_forecast_2 <- raster::extract(x = forecast_2,
                                      y = presence[, c("longitude", "latitude")],
                                      ID = FALSE) 

#une tabla de lats y longs con sus datos de clima
points_climate_recent <- cbind(presence, bioclim_extract_recent)
points_climate_tavg50 <- cbind(presence, bioclim_extract_tavg50)
points_climate_tavg70 <- cbind(presence, bioclim_extract_tavg70)


bioclim_forecast <- cbind(presence, bioclim_forecast)
bioclim_forecast_2 <- cbind(presence, bioclim_forecast_2)

bioclim_forecast_1<-bioclim_forecast[1:3]
bioclim_forecast_1
plot(bioclim_forecast_1)



##CÓDIGO UNO POR UNO  
###Codigo bueno solo que se reemplazó por la función 



#worldclim
#Natalia Said
#06/02/2024
#script downloads data form worldclim for recent (1970-2000) and future (2021-2080) projections. 
#it plots temperature in the wanted section with longs and lats provided. Finally it also delivers dataframe with temperatures.
#update:with function it can loop through the different models 

#extracts recent data form bioclim average temp. 1970-2000 (12 files each for one month of the year)
bioclim_1970 <- worldclim_global(var = "tavg", res = 2.5, path = "1970_2000/")

#2021-2040", "2041-2060", & "2061-2080"
forecast_21<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, time="2021-2040", var="bioc", res=2.5, path = "2021_2040/")
forecast_41<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, time="2041-2060", var="bioc", res=2.5, path = "2041_2060/")
forecast_61<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, time="2061-2080", var="bioc", res=2.5, path = "2061_2080/")


#lats and longs from farm and research center in NC
{
  latitude = c(35.89177,35.66645)
  longitude = c(-79.0181,-78.493247)
  
  
  #to expand the map 
  max_lat<-latitude[1]+(latitude[1]*0.05)
  min_lat<-latitude[2]-(latitude[2]*0.05)
  max_lon<-longitude[1]-(longitude[1]*0.05)
  min_lon<-longitude[2]+(longitude[2]*0.05)
  
  
  #dataframe que conecta con los 4 puntos 
  geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))
  
  
  #Download data with geodata's world function to use for our base map
  world_map <- world(resolution = 3,
                     path = "data/")
  
  
  # Crop the map to our area of interest
  my_map <- crop(x = world_map, y = geographic_extent)
  
}


# Crop bioclim data to desired extent
bioclim_1970<- crop(x = bioclim_1970, y = geographic_extent)
forecast_21<-crop(x = forecast_21, y = geographic_extent)
forecast_41<-crop(x = forecast_41, y = geographic_extent)
forecast_61<-crop(x = forecast_61, y = geographic_extent)

#mean of bioclim_1970 for the 12 months, produces only one graph
bioclim_1970<-mean(bioclim_1970, na.rm=FALSE)

#increases/decresases
{
  aumento_40_años<-forecast_61[[1]]-bioclim_1970
  aumento_20_años<-forecast_41-forecast_21
  
  plot(aumento_40_años)
}


#PLOT FOR TAVG RECENT 1970-2000
{ plot(bioclim_1970)
  title(main=paste0("Mean Temperature\n(1970 - 2000)"))
  place<-c("Farms","Research")
  text(longitude, latitude, labels=place, pos=2, cex=0.75)
  points(x = longitude, 
         y = latitude, 
         col = "red", 
         pch = 20, 
         cex = 1)
}
#PLOT FOR TAVG PROJECTED 2021-2040
{plot(forecast_21[[1]])
  title(main=paste0("Mean Temperature\n(2021 - 2040)"))
  place<-c("Farms","Research")
  text(longitude, latitude, labels=place, pos=2, cex=0.75)
  points(x = longitude, 
         y = latitude, 
         col = "red", 
         pch = 20, 
         cex = 1)
}
#PLOT FOR TAVG PROJECTED 2041-2060
{plot(forecast_41[[1]])
  title(main=paste0("Mean Temperature\n(2041 - 2060)"))
  place<-c("Farms","Research")
  text(longitude, latitude, labels=place, pos=2, cex=0.75)
  points(x = longitude, 
         y = latitude, 
         col = "red", 
         pch = 20, 
         cex = 1)
}
#PLOT FOR TAVG PROJECTED 2061-2080
{plot(forecast_61[[1]])
  title(main=paste0("Mean Temperature\n(2061 - 2080)"))
  place<-c("Farms","Research")
  text(longitude, latitude, labels=place, pos=2, cex=0.75)
  points(x = longitude, 
         y = latitude, 
         col = "red", 
         pch = 20, 
         cex = 1)
}


#df de longs y lats 
presence<-data.frame(longitude,latitude)

#Extracts climate info with the coordinate sets and converts SPATRASTER to dataframe
bioclim_recent_1970 <- terra::extract(x = bioclim_1970, y = presence[, c("longitude", "latitude")]) 
bioclim_forecast_21 <- terra::extract(x = forecast_21, y = presence[, c("longitude", "latitude")]) 
bioclim_forecast_41 <- terra::extract(x = forecast_41, y = presence[, c("longitude", "latitude")]) 
bioclim_forecast_61 <- terra::extract(x = forecast_61, y = presence[, c("longitude", "latitude")]) 

#une tabla de lats y longs con sus datos de clima
bioclim_recent_1970 <- cbind(presence, bioclim_recent_1970)
bioclim_forecast_21 <- cbind(presence, bioclim_forecast_21[2])
bioclim_forecast_41 <- cbind(presence, bioclim_forecast_41[2])
bioclim_forecast_61 <- cbind(presence, bioclim_forecast_61[2])


