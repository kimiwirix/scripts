#NORMA DE REACCION de individual growth curves in temps: 30,37,42

library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(ggtext)
library(ggsignif)


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
custom_colors <- c("Bacillus altitudinis"="#273a3eff", "Corynebacterium sp."="#08519cff",  "Staphylococcus arlettae"="#00e5eeff", "Bacillus thuringiensis"="#4682b4ff",  "Staphylococcus shinii"="#bdd7e7ff", "Bacillus atrophaeus"="#8c2424ff", "Micrococcus luteus"="#cd2626ff", "Bacillus infantis"="#ff0000ff", "Priestia megaterium"="#ef6d53ff", "Metabacillus indicus"="#fcae91ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Staphylococcus arlettae", "Bacillus thuringiensis", "Staphylococcus shinii", "Bacillus atrophaeus",  "Micrococcus luteus",  "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(paste0("italic(\"", strains, "\")"), strains)


t_AUC$Cepa  <- factor(t_AUC$Cepa, levels = strains)
t_mean$Cepa <- factor(t_mean$Cepa, levels = strains)


#plot
r_norm<-ggplot()+
  geom_point(data=t_AUC,aes( x = temp, y = AUC, group = rep), colour="peachpuff4")+
  geom_line(data = t_mean, aes( x = temp, y = mean_AUC, group = Cepa, color=Cepa), 
            linewidth = 1.5)+
  scale_color_manual(values = custom_colors)+
  facet_wrap(~Cepa, ncol = 5, labeller = as_labeller(italic, label_parsed))+
  labs( title = "Reaction norms",y=expression("AUC"), x = expression("Temperature °C"))+
  theme(legend.position = "none",                                            
        plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))   +
  geom_signif(
    data = subset(t_AUC, Cepa %in% c("Bacillus thuringiensis")),
    aes(x = temp, y = AUC),
    comparisons = list(c("30", "42")), annotations="*",
    map_signif_level = TRUE
  )+
  geom_signif(
    data = subset(t_AUC, Cepa %in% c("Bacillus atrophaeus", "Bacillus altitudinis")),
    aes(x = temp, y = AUC),
    comparisons = list(c("30", "37")), annotations="*",
    map_signif_level = TRUE
  )+
  geom_signif(
    data = subset(t_AUC, Cepa %in% c("Corynebacterium sp.", "Bacillus atrophaeus", "Bacillus infantis")),
    aes(x = temp, y = AUC),
    comparisons = list(c("30", "42")), annotations="***",
    map_signif_level = TRUE
  )+
  geom_signif(
    data = subset(t_AUC, Cepa %in% c("Micrococcus luteus")),
    aes(x = temp, y = AUC),
    comparisons = list(c("30", "37")), annotations="**",
    map_signif_level = TRUE
  )+
  geom_signif(
    data = subset(t_AUC, Cepa %in% c("Staphylococcus shinii", "Staphylococcus arlettae")),
    aes(x = temp, y = AUC),
    comparisons = list(c("30", "42")), annotations="***",
    map_signif_level = TRUE
  )
  
r_norm



ggsave(r_norm,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/gc_and_rxnnorm/rxn_norm.png" ,
       bg="white",  width = 40, height = 21, units = "cm")



#+ Descarga de df t_AUC y t_mean para usar en graficas en donde se hace la 
#+ comparación de las rxn norms de la comunidad con las rxn norms de las cepas 
#+ que componen la comunidad 

write.table(t_AUC, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/gc_and_rxnnorm/AUC_indiv_strains.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = FALSE)

write.table(t_mean, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/gc_and_rxnnorm/AUC_mean_indiv_strains.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = FALSE)

