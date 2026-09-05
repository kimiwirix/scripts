#grafica de comsint experimentos (qPCRs)
#eje x: días (d0,d1,d2) o con timepoints 
#eje y: qPCRS 

#+ Ajustes de volumen:
#+ Ensambles: de 1ml que sobró del protocolo ensambles hice extracciones y resuspendí
#+ en 100uL (diln1:10) y usé 1ull para hacer qPCR. las extracciones de DNA se 
#+ sacaron del mililitro restante del PASO10 de protocolo ensamble de tubo con 
#+ 2ml, esos 2ml se diluyeron 1:10 en PASO10.1 y luego se volvieron a diluir 1:2
#+ en PASO10.2 y al último se diluyeron 1:100 300ul:30ml. 
#+ 
#+ PLT PARA ENSAMBLES los frag/ml se tienen que dividir entre 20000. 


#+ Batches: de los 30ml de cultivo se extrajeron 2ml (vol/15),luego se hizo extraccion
#+ de esos 2ml y se resuspendio en 60uL. De esos 60uL se uso 1uL para hacer qPCR
#+ (vol/60).
#+ 
#+ PLT PARA BATCHES: los frag/ml se tienen que multiplicar por 15*60 (*900)


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


#+ Hace dos df. Uno para las muestras de los batches (b) y otra para los ensambles (e). 
#+ y modifcia ambos para que sean lo mas similares posibles 

b <- q %>%
  left_join(m, by ='label_final')%>%
  filter(!is.na(community)) %>%
  select(!c(label, batch, techrep))%>%
  mutate(Fragmentos_16S_ml = Fragmentos_16S_ml * 900)


#+ Pero hay un problema, los ensambles no tienen temp ni repbio, entonces 
#+ en temp_rep se recopiln todas las combinaciones de comunidad+temp+repbio 
#+ que son diferentes y eso se agrega con left join a los datos de los ensambles 
#+ para que se repitan las mediciones de cada comunidad tres veces por las temps
#+ y dos veces (o 3) por las repbios  

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")


temp_rep <- b %>%
  distinct(community, temp, repbio)


e <- q %>%
  left_join(m, by ='label_final')%>%
  filter(is.na(community))%>%
  mutate(community = communities) %>%
  mutate(timepoint=0, hrs =0) %>%
  select(Muestra, label_final, DNA_conc, Vol_qPCR, CT, date, Fragmentos_16S_ml,
         community, timepoint, hrs) %>%
  left_join(
    temp_rep,
    by = "community")%>%
  mutate(Fragmentos_16S_ml = Fragmentos_16S_ml/20000)




#+ All. joins both df 

a <- b %>%
  full_join(e)


#+ Arrengea las comunidades en el df por orden 
a$community <- factor(a$community, levels = communities)



#+ plot del crecimiento de cada comunidad en el tiempo en las tres 
#+ temperaturas con su réplica 

p <- ggplot()+
  geom_line(data = a, aes(x = hrs, y = log10(Fragmentos_16S_ml)  , colour = as.factor(temp), group = interaction(community, repbio, temp), linetype=as.factor(repbio)), linewidth=0.8)+
  facet_wrap(~community)+
  scale_color_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))+
  labs(title = "Community growth in temperature treatments", 
       y=expression("log10(16S/ml)"), 
       x = expression("Time"["hrs"]), 
       color="Temperature (°C)",
       linetype = "Replicates" )+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))

p

ggsave(p,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/qPCR/qpcr_plot.png",
       bg="white",  width = 30, height = 14, units = "cm")
