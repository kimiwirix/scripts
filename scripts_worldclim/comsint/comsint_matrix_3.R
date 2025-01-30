
#Contruye df a partir de datos de OD repetidos 3 veces por día y 10 días por comunidad 
# Matrix con 6 columnas: Nutriente(HN,MN,LN), Día(0-10), repetición (1,2,3), comunidad(1-8), OD, S0(3,6,12,24,48)


library(readxl)
library(writexl)

#Cambiar a pestaña: LN Biomass (OD), MN Biomass (OD), HN Biomass (OD) 
data <- read_excel("C:/Users/natal/Documents/R carpetas/worldclim/science.abm7841_data_s1/science.abm7841_data_s1.xlsx", sheet = "HN Biomass (OD)")
df <- as.data.frame(data)


#establecer nutrient LN,MN,HN
#establecer S0: 3,6,12,24
mat <- data.frame(Nutrient = rep("HN",264), 
                 Day = c(rep(0,3),rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3)), 
                 Rep = c(1,2,3),
                 Community= c(rep("C1",33),rep("C2",33),rep("C3",33),rep("C4",33),rep("C5",33),rep("C6",33),rep("C7",33),rep("C8",33)),
                 OD=NA,
                 S0=rep(24,264))
                 
z<-1  #contador

#Cambiar community de 1 a 8 
#Indices dependiendo de 3,6,12,24,48. Establecer indices: 1,2,3,4 
index<-which(df$`High nutrient`=="Community8"); index #cambiar del 8 al 1 cada vez
ind<-index[4]+1; ind #para no sacar el nombre de la comunidad 

#Loop para obtener datos en una tabla de 3x11
#pone el valor de la tabla en la columna OD del dataframe
for (i in seq(1,11)){
  for (x in seq(0,2)){
    mat$OD[z]<-df[ind+x,i]
    z<-z+1
  }
}                 



mat$OD<-as.numeric(mat$OD) #convierte a numeric 
write.csv(mat, file="HN_24.csv") #hace un file 






