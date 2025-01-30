
#Hace graficas de las comsints y las compara con el average de las singlebact (lineas punteadas) ver singlebact_mean.R, prodice dos graficas, plot_1 con NS1 y NS3 y plot_2 con NS2 y NS4
#grafica de comsint experimentos
#eje x: días (d0,d1,d2) o con timepoints 
#eje y: OD600

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)


#cambiar dependiendo el archivo 
data_1 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS1.ods", sheet = "OD600_real")
data_2 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS2.ods", sheet = "OD600_real")
data_3 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS3.ods", sheet = "OD600_real")
data_4 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS4.ods", sheet = "OD600_real")

od600_1 <- as.data.frame(data_1)
od600_2 <- as.data.frame(data_2)
od600_3 <- as.data.frame(data_3)
od600_4 <- as.data.frame(data_4)

od600_1<-rbind(od600_1,od600_3)
od600_2<-rbind(od600_2,od600_4)



data_singlestrains <- read_ods("C:/Users/natal/Documents/LIIGH/data_test_bacteria_comsint.ods", sheet = "OD600")
ns1 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS1.ods", sheet = "syncoms")
ns2 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS2.ods", sheet = "syncoms")

od600_ss <- as.data.frame(data_singlestrains)
ns1_comsints <- as.data.frame(ns1)
ns2_comsints <- as.data.frame(ns2)


#cambiar en for loop 
comsints_1<-c('R1','R2','R3','R4','R5','R6')
comsints_2<-c('R7','R8','R9','R10','R11','R12')

for (comsint in comsints_2){
  
  #selecciona las filas con las bcaterias del mismo nombre
  df_bact_1<-od600_2[od600_2$Community==comsint,] 
  
  #quita la columna con bact name y rename colnames 
  df_bact_1$Community<-NULL
  colnames(df_bact_1)<-c('temp','0','24','48','72')
  
  #adds medicion para poder juntar los 28 y los 32 de un solo color 
  df_bact_1$medicion<-c(1,2,3,4) 
  
  
  #meltea la data en tabla dependiendo de la medicion y temp 
  melt_data <- melt(df_bact_1, id = c('medicion', 'temp'), variable.name = 'timeline')

  
  #corta strain col con community col
  
  #variable<-paste('R',i,sep='')
  com_strains<-ns2_comsints[,c('strain',comsint)] #va loopeando y poniendo col de nombres con col de comunidad
  com_strains<-com_strains[complete.cases(com_strains),] #quita NAs
    
    
  comsint_info_cortada<-merge(com_strains,od600_ss, by='strain') #junta los dos dfs con el strain number
  colnames(comsint_info_cortada)<-c('strain','community','temp','0','24','48','72')
  comsint_info_cortada$community<-NULL
  df_melted <- melt(comsint_info_cortada,  id.vars = c('temp','strain'), variable.name = 'timeline')
  promedio<-aggregate(value ~ temp + timeline, data=df_melted, FUN=mean) #saca promedio de od de las bacterias en la comunidad por temperatura por timeline
    
  assign(comsint,promedio)
    
  
  plot_name<-paste('plot', comsint, sep='_')
  melt_data$temp<-as.factor(melt_data$temp)
  promedio$temp<-as.factor(promedio$temp)
  
 #buen plot  
  temp_plot<-ggplot(NULL, aes(timeline,value))+
    geom_line(data=melt_data, aes(group=medicion, colour=temp))+
    geom_line(data=promedio, aes(group=temp, colour=temp),linetype = "dashed")+
    scale_color_manual(values = c( "#3333FF", "#FF6600"))+
    ggtitle(comsint)+
    xlab('Total time (h)') + ylab('OD 600')+
    labs(colour ='Temperature (°C)')+
    theme(plot.title = element_text(hjust = 0.5))+
    ylim(0,1.5)
  
   
  #se queda   
  assign(plot_name, temp_plot)
}




 
#arrange all plots in one window, with shared legened
plot_1<-ggarrange(
  `plot_R1`,`plot_R2`,`plot_R3`,`plot_R4`, `plot_R5`, `plot_R6`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 3, # number of rows
  ncol=2
)


plot_2<-ggarrange(
  `plot_R7`,`plot_R8`,`plot_R9`,`plot_R10`, `plot_R11`, `plot_R12`, # list of plots
  labels = NULL, # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 3, # number of rows
  ncol=2
)


plot_1
plot_2

ggsave(plot_1, file="NS1_NS3_comparison_plot.png", width = 14, height = 14, units = "cm")
ggsave(plot_2, file="NS2_NS4_comparison_plot.png", width = 14, height = 14, units = "cm")


