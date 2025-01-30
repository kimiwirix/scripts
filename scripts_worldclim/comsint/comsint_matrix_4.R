
#Contruye matrix rows:species(48), col:#muestra a partir de los números de abundance por comunidad por dia por especie

library(readxl)
library(writexl)


data <- read_excel("C:/Users/natal/Documents/R carpetas/worldclim/science.abm7841_data_s1/science.abm7841_data_s1.xlsx", sheet = "LN species abundances")
df <- as.data.frame(data)


#solo correr la primera vez 
matrix_base<- matrix(0, 48, 40)
rownames(matrix_base)<-c("Species1","Species2","Species3","Species4","Species5","Species6","Species7","Species8","Species9","Species10","Species11","Species12",
                         "Species13","Species14","Species15","Species16","Species17","Species18","Species19","Species20","Species21","Species22","Species23","Species24",
                         "Species25","Species26","Species27","Species28","Species29","Species30","Species31","Species32","Species33","Species34","Species35","Species36",
                         "Species37","Species38","Species39","Species40","Species41","Species42","Species43","Species44","Species45","Species46","Species47","Species48")



column_names<-c()
for (i in c(3,6,12,24,48)){
  for (x in seq(1,8)){
    column_names<-append(column_names,paste("C",x,"_",i,sep = "")) 
  }
}

colnames(matrix_base)<-column_names
n<-1


index<-which(df$`Low nutrient`=="Community1"); index #cambiar del 8 al 1 cada vez
ind<-index[5]+1; ind #para no sacar el nombre de la comunidad 

df_nuevo<-df[ind:(ind+47),12];df_nuevo #cambiar 3,6,12,24,48 



for (specie in df_nuevo){
  for(row_num in 1:48){
    if (specie==rownames(matrix_base)[row_num]){
      matrix_base[row_num,n]<-1
    }
  }
}
n<-n+1

x


matrix_base<-matrix_base[,-40:-34]
write.csv(matrix_base, file="mat4_LN.csv") #hace un file 


