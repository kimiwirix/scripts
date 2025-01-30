#Contruye matrix rows:species(48), col:#muestra a partir de los números de abundance por comunidad por dia por especie

library(readxl)
library(writexl)


data <- read_excel("C:/Users/natal/Documents/R carpetas/worldclim/science.abm7841_data_s1/science.abm7841_data_s1.xlsx", sheet = "MN species abundances")
df <- as.data.frame(data)


#solo correr la primera vez 
matrix_base<- matrix(0, 48, 88)  #rellena la matrix de 0
#matrix_base<- matrix(0, 48, 11) #para el de 48 
#matrix_base<- matrix(0, 48, 32) #para el MN 
rownames(matrix_base)<-c("Species1","Species2","Species3","Species4","Species5","Species6","Species7","Species8","Species9","Species10","Species11","Species12",
                         "Species13","Species14","Species15","Species16","Species17","Species18","Species19","Species20","Species21","Species22","Species23","Species24",
                         "Species25","Species26","Species27","Species28","Species29","Species30","Species31","Species32","Species33","Species34","Species35","Species36",
                         "Species37","Species38","Species39","Species40","Species41","Species42","Species43","Species44","Species45","Species46","Species47","Species48")
colnames(matrix_base)<-seq(96,183)  #0,87  88,175  176,263


#busca el index de la celda con nombre comunidad y le suma uno para sacar la celda de abajo con los datos 
#si cambia
index<-which(df$`Medium nutrient`=="Community5"); index #cambiar del 8 al 1 cada vez
ind<-index[4]+1; ind #cambiar los indeices dependiendo de 3,6,12,24, 48. para no sacar el nombre de la comunidad 


#hace un nuevo df para seleccionar la tabla del excel 
#espera cambio
df_nuevo<-df[ind:(ind+23),1:12] #cambiar 3,6,12,24,48 
#df_nuevo<-df[ind:(ind+23),1:5]


#si cambio
rownames(df_nuevo)<-df_nuevo[,12] #pone de rownames el nombre de las species
df_nuevo<-df_nuevo[,-12] #quita la columna con el nombre de las especies 
colnames(df_nuevo)<-seq(118,128); df_nuevo #0,10  11,21  22,32  33,43 

#rownames(df_nuevo)<-df_nuevo[,5]
#df_nuevo<-df_nuevo[,-5]
#colnames(df_nuevo)<-seq(92,95); df_nuevo #de 3 en 3


#no cambio, si correr 
matrix_nuevo<-as.matrix(df_nuevo)#convierte df a matrix y los datos a numeric
class(matrix_nuevo) <- "numeric"
matrix_nuevo


#matchea los datos en matrix nuevo que son las tablas del excel con los nombres de col y row para poner el numero indicado en la celd

}


x

#renommbra las colnames con M + numero
column_names<-c()
for (i in seq(65,96)){ #cambiar 
  column_names<-append(column_names, paste("M",i))
}

colnames(matrix_base)<-column_names
write.csv(matrix_base, file="MN_abundance_12.csv")



