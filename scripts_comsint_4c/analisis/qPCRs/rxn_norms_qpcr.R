#+ NORMAs DE REACCION de qPCRs 

library(readODS)
library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(ggtext)
library(ggsignif)
library(tidyr)



#+ Master dbs from data transformed in 
#+ "C:/Users/natal/Documents/LIIGH/scripts/scripts_comsint_4c/dbs/dbs_qPCRs.R"
#+ This dbs junta todos los batches en un solo df agregando datos de ensambles 
#+ y metadata

qpcr <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv", header = TRUE,  sep='\t' )


q <- qpcr %>%
  pivot_longer(cols = A_Fragmentos_16S_mL:C_Fragmentos_16S_ml,
               names_to = "std_curves", values_to = "Fragmentos_16S_ml")




#+ Primera parte añade el AUC de cada comunidad en cada temperatura a lo
#+ largo del tiempo pero quedan valores repetidos para cada tiempo, plt
#+ segunda parte elimina las filas repetidas dejando 1 fila por comsint por temp
#+ por réplica
#+ 
#+ Agrupa los datos por comunidad, temperatura, repbio y dentro de cada repbio hay 
#+ 3 std curves. 

q_AUC<-q%>%
  group_by(community, repbio, temp, std_curves)%>%
  arrange(hrs, .by_group = TRUE) %>%
  mutate(AUC=trapz(hrs, Fragmentos_16S_ml))%>%
  ungroup()%>%
  group_by(community, repbio, temp, std_curves) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop')




#+ convierte las temperaturas y repbios en factores, arrangea por orden las 
#+ comunidades en los dfs y  añade una colummna en donde
#+ se extrae el numero de comunidad para treating comms as continuos and be 
#+ able to get the gradient legend 

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
q_AUC$temp<-as.factor(q_AUC$temp)
q_AUC$repbio<-as.factor(q_AUC$repbio)
q_AUC$community <- factor(q_AUC$community, levels = communities)
q_AUC$community_num <- as.numeric(sub("C", "", q_AUC$community))


#+ plot of all reaction norms with replicates in one window with a gradient
#+ of color for communities 

rxn_norm_1<- ggplot()+
  geom_point(data= q_AUC, aes(x = temp, y = AUC), colour="peachpuff4") +
  geom_line(data = q_AUC, aes(x=temp, y= AUC, group=interaction(community, repbio, std_curves), colour = community_num), linewidth=1)+
  scale_color_gradient(low = "blue", high = "red") +
  scale_x_discrete(expand = c(0, 0))+
  labs( title = "Community reaction norms (qPCR)",y=expression("AUC"), x = expression("Temperature °C"), color="Communities")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) 


rxn_norm_1  
ggsave(rxn_norm_1,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/qPCR/rxn_norms_qpcr_2.png" ,
       bg="white",  width = 40, height = 21, units = "cm")




#+ plot separando por comunidad y con una linea por rplica por std_curves PLT 
#+ en casi todas las comsints 6 lineas (en las C3, C24 y C27 hay 9 lineas)

rxn_norm_2 <- ggplot()+
  geom_point(data = q_AUC, aes(x=temp, y=AUC), colour="peachpuff4")+
  geom_line(data= q_AUC, aes(x=temp, y=AUC, group = interaction (repbio, std_curves), colour=community_num, linetype = repbio), linewidth=1.2) +
  scale_color_gradient(low = "blue", high = "red") +
  facet_wrap(~community, ncol=8)+
  scale_x_discrete(expand = c(0, 0))+
  labs( title = "Community reaction norms (qPCR)",y=expression("AUC"), x = expression("Temperature °C"), color="Communities")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) 


rxn_norm_2
ggsave(rxn_norm_2,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/qPCR/rxn_norms_qpcr_1.png" ,
       bg="white",  width = 50, height = 21, units = "cm")



