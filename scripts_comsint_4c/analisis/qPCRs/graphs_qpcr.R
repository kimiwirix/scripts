#grafica de comsint experimentos (qPCRs)
#eje x: días (d0,d1,d2) o con timepoints 
#eje y: qPCRS 

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)


#+ Master dbs from metadata and qpcrs in folder: 
#+ C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ 
#+ assembled from dbs.R

q <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv", header = TRUE,  sep='\t' )
m <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv", header = TRUE,  sep='\t' )


#+ Junta las dos dbs para que qpcr tenga datos de metadata
b <- q %>%
  left_join(m, by ='label_final')



#+ Arrengea las comunidades en el df por orden 

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
b$community <- factor(b$community, levels = communities)



#+ plot del crecimiento de cada comunidad en el tiempo en las tres 
#+ temperaturas con su réplica 

p <- ggplot()+
  geom_line(data = b, aes(x = hrs, y = Fragmentos_16S_ml  , colour = as.factor(temp), group = interaction(community, repbio, temp), linetype=as.factor(repbio)), linewidth=0.8)+
  facet_wrap(~community)+
  scale_color_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))+
  labs(title = "Community growth in temperature treatments", 
       y=expression("16S/ml"), 
       x = expression("Time"["hrs"]), 
       color="Temperature (°C)",
       linetype = "Replicates" )+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))

p

ggsave(p,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/qPCR/qpcr_plot.png",
       bg="white",  width = 30, height = 14, units = "cm")
