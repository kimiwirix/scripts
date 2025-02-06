#graficar promedio de bacterias individuales 
 

library(readODS)
library(ggplot2) 
library(reshape2)
library(tidyverse)
library(ggpubr)


data_singlestrains <- read_ods("C:/Users/natal/Documents/LIIGH/data_test_bacteria_comsint.ods", sheet = "OD600")
od600_1 <- as.data.frame(data_singlestrains)

ns1 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS1.ods", sheet = "syncoms")
ns2 <- read_ods("C:/Users/natal/Documents/LIIGH/data_NS2.ods", sheet = "syncoms")

ns1_comsints <- as.data.frame(ns1)
ns2_comsints <- as.data.frame(ns2)

#corta strain col con community col
for (i in 7:12){
  variable<-paste('R',i,sep='')
  com_strains<-ns2_comsints[,c('strain',variable)] #va loopeando y poniendo col de nombres con col de comunidad
  com_strains<-com_strains[complete.cases(com_strains),] #quita NAs


  comsint_info_cortada<-merge(com_strains,od600_1, by='strain') #junta los dos dfs con el strain number
  colnames(comsint_info_cortada)<-c('strain','community','temp','0','24','48','72')
  comsint_info_cortada$community<-NULL
  df_melted <- melt(comsint_info_cortada,  id.vars = c('temp','strain'), variable.name = 'timeline')
  promedio<-aggregate(value ~ temp + timeline, data=df_melted, FUN=mean) #saca promedio de od de las bacterias en la comunidad por temperatura por timeline

  assign(variable,promedio)



  #PLOT
  plot_name<-paste('plot', variable, sep='_')
  promedio$temp<-as.factor(promedio$temp)

  temp_plot<-ggplot(promedio, aes(timeline,value)) +
    geom_point()+
    scale_color_manual(values = c( "#3333FF", "#FF6600"))+
    geom_line(
      aes(group=temp, colour=temp))+
    ggtitle(variable)+
    xlab('Total time (h)') + ylab('OD 600')+
    labs(colour ='Temperature (°C)')+
    theme(plot.title = element_text(hjust = 0.5))+
    ylim(0,1.5)

  assign(plot_name, temp_plot)
}

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

ggsave(plot_1, file="NS1_NS3_singlebact_average_plot.png", width = 14, height = 14, units = "cm")
ggsave(plot_2, file="NS2_NS4_singlebact_average_plot.png", width = 14, height = 14, units = "cm")



