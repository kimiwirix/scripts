#NORMA DE REACCION de individual growth curves in temps: 30,37,42

library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(ggtext)


t<-read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_filtered.tsv", 
              sep = '\t', 
              header = TRUE)

#AUC addition and eliminate filas que se repitan 
t_AUC<-t%>%
  group_by(Cepa, rep, temp)%>%
  mutate(AUC=trapz(hr,`OD.real`))%>%
  ungroup()%>%
  group_by(Cepa, rep, temp, Incubator) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop')%>%
  mutate(Cepa = recode(Cepa,
                       "CH23"  = "Bacillus altitudinis",
                       "CH29"  = "Corynebacterium sp.",
                       "CH90"  = "Bacillus atrophaeus",
                       "CH99b" = "Staphylococcus arlettae",
                       "CH111" = "Bacillus thuringiensis",
                       "CH149a"= "Micrococcus luteus",
                       "CH154a"= "Staphylococcus shinii",
                       "CH161d"= "Bacillus infantis",
                       "CH447" = "Priestia megaterium",
                       "CH450" = "Metabacillus indicus"))


t_AUC$temp<-as.factor(t_AUC$temp)


#t_mean to compute the geom line that shows the mean of the three replicates for each temp 
t_mean <- t_AUC %>%
  group_by(Cepa, temp) %>%
  summarise(mean_AUC = mean(AUC, na.rm = TRUE), .groups = "drop")



communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
custom_colors <- c("Bacillus altitudinis"="#ff0000ff", "Corynebacterium sp."="#cd2626ff", "Bacillus atrophaeus"="#fcae91ff", "Staphylococcus arlettae"="#4682b4ff", "Bacillus thuringiensis"="#8c2424ff",
                   "Micrococcus luteus"="#00e5eeff", "Staphylococcus shinii"="#ef6d53ff", "Bacillus infantis"="#08519cff", "Priestia megaterium"="#273a3eff", "Metabacillus indicus"="#bdd7e7ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Bacillus atrophaeus", "Staphylococcus arlettae", "Bacillus thuringiensis","Micrococcus luteus", "Staphylococcus shinii", "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(paste0("italic(\"", strains, "\")"), strains)

#plot
r_norm<-ggplot()+
  geom_point(data=t_AUC,aes( x = temp, y = AUC, group = rep), colour="peachpuff4")+
  geom_line(data = t_mean, aes( x = temp, y = mean_AUC, group = Cepa, color=Cepa), 
            linewidth = 1)+
  scale_color_manual(values = custom_colors)+
  facet_wrap(~Cepa, ncol = 5, labeller = as_labeller(italic, label_parsed))+
  labs( title = "Reaction norms",y=expression("AUC"), x = expression("Temperature °C"))+
  theme(legend.position = "none",                                            
        plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))          

r_norm


ggsave(r_norm,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/gc_and_rxnnorm/rxn_norm.png" ,
       bg="white",  width = 30, height = 12, units = "cm")
