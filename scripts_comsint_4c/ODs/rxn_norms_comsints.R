#+ NORMAs DE REACCION de comsints 

library(readODS)
library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(ggtext)
library(ggsignif)
library(tidyr)



#+ Master dbs from data in 
#+ C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods 
#+ This dbs junta todos los batches en un solo df en ODs_db.R

b <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ODs_db.tsv", header = TRUE,  sep='\t' )



#+ Primera parte añade el AUC de cada comunidad en cada temperatura a lo
#+ largo del tiempo pero quedan valores repetidos para cada tiempo, plt
#+ segunda parte elimina las filas repetidas dejando 1 fila por comsint por temp
#+ por réplica
 
b_AUC<-b%>%
    group_by(community, repbio, temp)%>%
  mutate(AUC=trapz(hrs,OD_real))%>%
  ungroup()%>%
  group_by(community, repbio, temp) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop')



#+ t_mean computes the mean between the replicates for each measurement of 
#+ community and temperature trhoughout time

b_mean <- b_AUC %>%
  group_by(community, temp) %>%
  summarise(mean_AUC = mean(AUC, na.rm = TRUE), .groups = "drop")



#+ convierte las temperaturas de lso dos dfs en factores, arrangea por orden las 
#+ comunidades en los dfs y para los dos dfs, añade una colummna en donde
#+ se extrae el numero de comunidad para treating comms as continuos and be 
#+ able to get the gradient legend 

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
b_AUC$temp<-as.factor(b_AUC$temp)
b_mean$temp<-as.factor(b_mean$temp)
b_AUC$community <- factor(b_AUC$community, levels = communities)
b_mean$community <- factor(b_mean$community, levels = communities)
b_mean$community_num <- as.numeric(sub("C", "", b_mean$community))
b_AUC$community_num <- as.numeric(sub("C", "", b_AUC$community))


#+ plot of all reaction norms with replicates in one window with a gradient
#+ of color for communities 

rxn_norm_1<- ggplot()+
        geom_point(data= b_AUC, aes(x = temp, y = AUC, group = repbio), colour="peachpuff4")+
        geom_line(data = b_AUC, aes(x=temp, y= AUC, group=interaction(community, repbio), colour = community_num), linewidth=1)+
        scale_color_gradient(low = "blue", high = "red") +
        scale_x_discrete(expand = c(0, 0))+
        labs( title = "Community reaction norms",y=expression("AUC"), x = expression("Temperature °C"), color="Communities")+
        theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) 
  

rxn_norm_1  
ggsave(rxn_norm_1,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/ODs/rxn_norms_comsints_2.png" ,
       bg="white",  width = 40, height = 21, units = "cm")






#+ plot separando por comunidad y con una linea por réplica y añadiendo 
#+ rxn norm media.  

rxn_norm_2 <- ggplot()+
        geom_point(data = b_AUC, aes(x=temp, y=AUC, group = repbio), colour="peachpuff4")+
        geom_line(data= b_AUC, aes(x=temp, y=AUC, group = repbio, colour=as.factor(repbio)), linewidth=1.5)+
       # scale_color_gradient(low = "blue", high = "red") +
        geom_line(data=b_mean, aes(x=temp,y=mean_AUC, group = community),linetype='dotted', linewidth=0.8)+
        facet_wrap(~community, ncol=8)+
        scale_x_discrete(expand = c(0, 0))+
        labs( title = "Community reaction norms",y=expression("AUC"), x = expression("Temperature °C"), color="Communities")+
        theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) 


rxn_norm_2
ggsave(rxn_norm_2,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/ODs/rxn_norms_comsints_1_edited_DNAextr.png" ,
       bg="white",  width = 50, height = 21, units = "cm")




#+ Esto calcula la correlacion entre las replicas de las rxn norms 
#+ Calula correlacion entre rep1 y rep2, rep2 y rep3, rep1 y rep3 y también 
#+ un promedio de los tres coeficientes
#+ los valores cerca de 1 es porque ambas replicas se parecen mas 


corr <- b_AUC %>%
  pivot_wider(names_from = repbio, values_from = AUC) %>%
  group_by(community) %>%
  summarise(
    cor12 = cor.test(`1`, `2`, method = 'pearson')$estimate,
    pval12 = cor.test(`1`, `2`, method = 'pearson')$p.value,
    cor13 = cor.test(`1`, `3`, method = 'pearson')$estimate,
    pval13 = cor.test(`1`, `3`, method = 'pearson')$p.value,
    cor23 = cor.test(`2`, `3`, method = 'pearson')$estimate,
    pval23 = cor.test(`2`, `3`, method = 'pearson')$p.value,
    mean_cor = mean(c(cor12, cor13, cor23), na.rm = TRUE)
  )
corr
