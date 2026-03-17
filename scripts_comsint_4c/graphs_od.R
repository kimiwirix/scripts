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
data<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_data_od.ods", sheet = "repbioA" )%>%
  select(!notes)%>%
  as.data.frame()


#METADATA 
m<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_data_collection.ods", sheet = "repbioA")%>%
  select(!c(notes, date, time))%>%
  as.data.frame()

d<-data%>%
  left_join(m)%>%
  filter(!is.na(OD600))%>%
  select(c(community,repbio,temp,timepoint,hrs,OD_real))


head(d)

ggplot(d, aes(x=hrs, y=OD_real, colour = as.factor(temp), linetype="Synthetic communities"), size=0.8)+
  geom_line()+
  facet_wrap(~community)+
  labs(title = "Growth: Individual vs Communities  ", y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), color="Temperature (°C)", linetype="Growth")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))







