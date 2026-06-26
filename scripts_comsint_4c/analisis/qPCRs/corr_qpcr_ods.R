#+ CORRELATION between od measurmentes and qPCR measurements 


library(readODS)
library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
library(ggpmisc)
library(cowplot)




#+ Master dbs from data in dbs.R
#+ This dbs junta todos los batches en un solo df en dbs.R
#+ In o: since qpcr is based on the sequenced genomes there is no timepoint 0 and 1 
#+ In q: joins to qpcr results the metadata 
#+ 
#+ At the end for each community, temp, timepoint and repbio there is one
#+ od measurement and one qpcr measurement 

o <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ODs_db.tsv", header = TRUE,  sep='\t' )%>%
  filter(!(hrs==0 | hrs==6))%>%
  select(community, repbio, temp, timepoint, hrs, OD_real, batch)

m <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv", header = TRUE,  sep='\t' )

q <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv", header = TRUE,  sep='\t' )%>%
  left_join(m, by ='label_final')%>%
  select(!c(Muestra, DNA_conc,Vol_qPCR, CT, date, techrep, label, label_final))



#+ Junta las dos dbs para e un solo df n 
n <- q %>%
  left_join(o, by = c('community', 'repbio', 'temp', 'timepoint', 'hrs'))




# WTFFFF PREGUNTAR SUR 

correlation <- n %>%
  group_by(community) %>%
  summarise(
    cor12 = cor(OD_real,log10(Fragmentos_16S_ml), method = 'pearson'))
correlation




#+ plot comparando los fragmentos16S/ml contra las mediciones de OD donde cada punto 
#+ es un label (comunidad x temp x timepoint x repbio)
#+ 
#+ 
#+ NOTA: VOLVER A HACER UNA VEZ QUE SE HAYA AJUSTADO EL VALOR REAL DE FRAGMENTOS 
#+ BASADOS EN EL NUMERO DE COPIAS DEL 16S EN CADA CEPA
#+ 
#+ If colour against Tp we can see clearly the divisions


p <- ggplot()+
  geom_point(data = n, aes(x = OD_real, y = log10(Fragmentos_16S_ml), colour=as.factor(temp))) +
  stat_smooth(data=n, aes(x = OD_real, y = log10(Fragmentos_16S_ml )), method = 'lm', formula = y~x, inherit.aes = FALSE)+
  stat_correlation(data=n, aes(x = OD_real, y = log10(Fragmentos_16S_ml )), inherit.aes = FALSE)+
  labs (title = expression ("Correlation between 16S fragments/ml and OD"["600nm"]),
    y="16S/ml",
    color="Timepoint")





p
ggsave(p,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/qPCR/correlations_qpcr_and_OD.png" ,
       bg="white",  width = 50, height = 21, units = "cm")













