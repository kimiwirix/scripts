#NORMA DE REACCION de individual growth curves in temps: 30,37,42

library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(ggtext)


t<-read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW_filtered.tsv", 
              sep = '\t', 
              header = TRUE)

#AUC addition and eliminate filas que se repitan 
t_AUC<-t%>%
  group_by(Cepa, rep, temp)%>%
  mutate(AUC=trapz(hr,`OD.real`))%>%
  ungroup()%>%
  group_by(Cepa, rep, temp, Incubator) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop')


t_AUC$temp<-as.factor(t_AUC$temp)  


#t_mean to compute the geom line that shows the mean of the three replicates for each temp 
t_mean <- t_AUC %>%
  group_by(Cepa, temp) %>%
  summarise(mean_AUC = mean(AUC, na.rm = TRUE), .groups = "drop")



real_names<-c('CH23'='<i>Bacillus altitudinis</i>', 'CH29'='<i>Corynebacterium sp.</i>', 'CH90'='<i>Bacillus atrophaeus</i>',  
              'CH99'='<i>Staphylococcus arlettae</i>', 'CH111'='<i>Bacillus thuringiensis</i>', 'CH149a'='<i>Micrococcus luteus</i>', 
              'CH154a'='<i>Staphylococcus shinii</i>', 'CH161d'='<i>Bacillus infantis</i>',  'CH447'='<i>Priestia megaterium</i>', 'CH450'='<i>Metabacillus indicus</i>')

colors<-c('CH23'='#25383cfb', 'CH29'='#08519cff', 'CH90'='#8c2424ff', 'CH99'='#00e5eeff', 'CH111'='#4682b4ff', 'CH149a'='#cd2626ff', 
          'CH154a'='#bdd7e7fc', 'CH161d'='#ff3030ff',  'CH447'='#ee6a50fa', 'CH450'='#fcae91ff')


#plot
r_norm<-ggplot()+
  geom_point(data=t_AUC,aes( x = temp, y = AUC, group = rep), colour="peachpuff4")+
  geom_line(data = t_mean, aes( x = temp, y = mean_AUC, group = Cepa, color=Cepa), 
            linewidth = 1)+
  scale_color_manual(values = colors)+
  facet_wrap(~Cepa, ncol=5, labeller = as_labeller(real_names))+
  labs( title = "Reaction norms",y=expression("AUC"), x = expression("Temperature °C"))+
  theme(strip.text = element_markdown(),
        legend.position = "none",                                                # removes the legend
        plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))            # enables <i></i> formatting


r_norm


ggsave(r_norm,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/rxn_norm.png" ,
       bg="white",  width = 30, height = 12, units = "cm")
