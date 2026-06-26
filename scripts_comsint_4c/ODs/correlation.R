#+ CORRELATION between od measurmentes (community,temp, timepoint)
#+ CORRELATION between rxn norms 
#+ Both in one plot 


library(readODS)
library(dplyr)
library(tidyr)
library(pracma)
library(ggplot2)
library(ggpmisc)
library(cowplot)




#+ Master dbs from data in 
#+ C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods 
#+ This dbs junta todos los batches en un solo df en ODs_db.R

b <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ODs_db.tsv", header = TRUE,  sep='\t' )



#+ ESTO ES PARA DATOS DE RXN NORMS 
#+ 
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
            .groups = 'drop')%>%
  pivot_wider(names_from=repbio, values_from=AUC)
  
  

b_AUC


#+ cambia el df a wider para agrgar las columnas 1 y 2 que pertenecen a las
#+ reps 

n <- b%>%
    select(community, temp, timepoint, OD_real, repbio)%>%
    pivot_wider(names_from=repbio, values_from=OD_real)%>%
    filter(!timepoint==0)



#+ calcula la correlación entre las tres replicas de cada comunidad en una 
#+ temperatura a lo largo del tiempo. Las correlaciones son altas en todas pero 
#+ cuando calculo la correlacion en las normas de rxn, hay valores muy bajos,
#+ esto quiere decir que within temperaturas si se parecen las replicas pero el 
#+ cambio del fenotipo a diferentes temperaturas no es muy reproducible 
#+ 
#+ 
#+ 
#+ PORQUE CPRRLEACIONES ENTRE COMMS Y TEMP SON ALTAS Y ENTRE RXN NORMS SON BAJAS 

correlation <- n %>%
  group_by(community, temp) %>%
  summarise(
    cor12 = cor(`1`, `2`, method = 'pearson'),
    cor13 = cor(`1`, `3`, method = 'pearson'),
    cor23 = cor(`2`, `3`, method = 'pearson'),
    mean_cor = mean(c(cor12, cor13, cor23), na.rm = TRUE)
  )
n




#+ 3 plots para n: comparando la replica 1 vs 2, 1 vs 3, 2 vs 3
#+ y cada punto es una comunidad en una temperatura en un tiempo especifico. 
#+ 
#+ NOTA: antes de hacer los plots quitar la scale_y_continous para ver si 
#+ se estan incluyendo todos los puntos 


p1<-ggplot()+
    geom_point(data = n, aes(x = `1`, y = `2`)) +
    scale_y_continuous(limits = c(0,1.3),breaks = seq(0, 1.4, by = 0.20)) +
    stat_smooth(data=n, aes(x=`1`,y = `2`), method = 'lm', formula = y~x, inherit.aes = FALSE)+
    stat_correlation(data=n, aes(x=`1`,y = `2`), inherit.aes = FALSE)
  
p2<-ggplot()+
    geom_point(data = n, aes(x = `1`, y = `3`)) +
    scale_y_continuous(limits = c(0,1.3),breaks = seq(0, 1.4, by = 0.20))  +
    stat_smooth(data=n, aes(x=`1`,y = `3`), method = 'lm', formula = y~x, inherit.aes = FALSE)+
    stat_correlation(data=n, aes(x=`1`,y = `3`), inherit.aes = FALSE)

p3<-ggplot()+
   geom_point(data = n, aes(x = `2`, y = `3` )) +
   scale_y_continuous(limits = c(0,1.3),breaks = seq(0, 1.4, by = 0.20))  +
   stat_smooth(data=n, aes(x=`2`,y = `3`), method = 'lm', formula = y~x, inherit.aes = FALSE)+
   stat_correlation(data=n, aes(x=`2`,y = `3`), inherit.aes = FALSE)





#+ 3 plots para b_AUC: comparando la replica 1 vs 2, 1 vs 3, 2 vs 3
#+ y cada punto es un area bajo la curva (una comunidad en una temperatura 
#+ a lo largo del tiempo)
#+ 
#+ NOTA: antes de hacer los plots quitar la scale_y_continous para ver si 
#+ se estan incluyendo todos los puntos 


p4<-ggplot()+
  geom_point(data = b_AUC, aes(x = `1`, y = `2`, colour=as.factor(temp)))+
  scale_color_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))+
  scale_y_continuous(limits = c(8,18),breaks = seq(8,18, by = 2))  +
  stat_smooth(data=b_AUC, aes(x=`1`,y = `2`), method = 'lm', formula = y~x, inherit.aes = FALSE)+
  stat_correlation(data=b_AUC, aes(x=`1`,y = `2`), inherit.aes = FALSE)+
  labs (color="Temperature °C")

p5<-ggplot()+
  geom_point(data = b_AUC, aes(x = `1`, y = `3`, colour=as.factor(temp)))+
  scale_color_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))+
  scale_y_continuous(limits = c(8,18),breaks = seq(8,18, by = 2))  +
  stat_smooth(data=b_AUC, aes(x=`1`,y = `3`), method = 'lm', formula = y~x, inherit.aes = FALSE)+
  stat_correlation(data=b_AUC, aes(x=`1`,y = `3`), inherit.aes = FALSE)
  
p6<-ggplot()+
    geom_point(data = b_AUC, aes(x = `2`, y = `3`, colour=as.factor(temp))) +
    scale_color_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))+
    scale_y_continuous(limits = c(8,18),breaks = seq(8,18, by = 2))  +
    stat_smooth(data=b_AUC, aes(x=`2`,y = `3`), method = 'lm', formula = y~x, inherit.aes = FALSE)+
    stat_correlation(data=b_AUC, aes(x=`2`,y = `3`), inherit.aes = FALSE)




#+ Junta los 6 plots en una ventana usando plot grid para incluir título, plots, 
#+ titulo y plots y al final le da formato al texto para que aparezca en medio
#+ quita las leyendas para que no salga cada plot con su leyenda 


corr_plot <- plot_grid(
        ggdraw() +
          draw_label(
            "OD correlation between replicates",
            fontface = "bold",
            size = 14),
        
        plot_grid(p1 + theme(legend.position = 'none'), 
                  p2 + theme(legend.position = 'none'), 
                  p3 + theme(legend.position = 'none'), nrow = 1),
        
        ggdraw() +
          draw_label(
            "AUC correlation between replicates",
            fontface = "bold",
            size = 14),
        
        plot_grid(p4 + theme(legend.position = 'none'), 
                  p5 + theme(legend.position = 'none'), 
                  p6 + theme(legend.position = 'none'), nrow = 1),
        
        ncol = 1, 
        rel_heights = c(0.08, 1, 0.08, 1))





#+ saca la leyenda de uno de los plots porque es la misma para todos y la 
#+ guarda en la variable

leyenda<-get_legend(
  p4 + theme(legend.box.margin = margin(0, 0, 0, 12)))




#+ junta el plot de los 6 plots con la leyenda 
p <- plot_grid(corr_plot, leyenda, nrow = 1, rel_widths = c(1, 0.1))



p
ggsave(p,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/ODs/correlations_AUC_and_OD.png" ,
       bg="white",  width = 50, height = 21, units = "cm")













