#se hizo un experimento con 3 comunidades en una temp (37°C) para saber si en los ultimos timepoints hay dominancia de una sola cepa
#COCLUSION: vamos a meter en el proximo batch (batch1) un tiempoint intermedio 1.5 (9 hrs) para ver si cambiamos el último timepoint por ese 

library(readODS)
library(dplyr)
library(ggplot2)


c1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CFUs_dominance_assay.ods", sheet = "C1")
c17<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CFUs_dominance_assay.ods", sheet = "C17")
c31<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CFUs_dominance_assay.ods", sheet = "C31")

f<-c1%>%
  bind_rows(c17,c31)%>%
  mutate(CFUs=coalesce(CFUs,0))%>%
  group_by(community, timepoint, strain)%>%
  summarise(s=sum(CFUs), 
            v=sum(`volume_plated(ml)`),
            d=1/dilution)%>%
  mutate(cfus_ml=s*d/(v))%>%
  mutate(strain = recode(strain,
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


custom_colors <- c("Bacillus altitudinis"="#273a3eff", "Corynebacterium sp."="#08519cff", "Bacillus atrophaeus"="#8c2424ff", "Staphylococcus arlettae"="#00e5eeff", "Bacillus thuringiensis"="#4682b4ff","Micrococcus luteus"="#cd2626ff", "Staphylococcus shinii"="#bdd7e7ff", "Bacillus infantis"="#ff0000ff", "Priestia megaterium"="#ef6d53ff", "Metabacillus indicus"="#fcae91ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Bacillus atrophaeus", "Staphylococcus arlettae", "Bacillus thuringiensis","Micrococcus luteus", "Staphylococcus shinii", "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(lapply(strains, function(x) bquote(italic(.(x)))), strains)


#plot with suspected numeber of cfus by counts on plate
p<-ggplot(data = f, aes(x= timepoint, y = log10(cfus_ml), fill = strain))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values =custom_colors,
                    labels=italic) +
  facet_wrap(~community, ncol=1) +
  geom_vline(
    xintercept = seq(1.5, length(unique(f$timepoint)) - 0.5, 1),
    linetype = "dashed",color = "grey70")+
  labs(title="CFUs/ml in communities")+
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))

p
ggsave(p,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/cfus_dominance_plates_assay.png" ,
       bg="white",  width = 30, height = 14, units = "cm")


#plot with numero de fenotipos diferentes
f1<-c1%>%
  bind_rows(c17,c31)%>%
  filter(!is.na(CFUs))%>%
  group_by(community, timepoint)%>%
  summarise(phenotypes = n_distinct(strain)) %>%   # Now summarise with unique elements per group
  ungroup()


p1<-ggplot(data = f1, aes(x= timepoint, y = phenotypes))+
  geom_point()+
  geom_bar(stat = "identity", position = "dodge", fill="skyblue4")+
  facet_wrap(~community, ncol=1) +
  geom_vline(
    xintercept = seq(1.5, length(unique(f$timepoint)) - 0.5, 1),
    linetype = "dashed",color = "grey70")+
  scale_y_continuous(limits=c(0,5), breaks=seq(0,5, by = 1))+
  labs(title="Number of phenotypes observed per community")+
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))

ggsave(p1,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/cfus_dominance_phenotypes_observed.png" ,
       bg="white",  width = 14, height = 14, units = "cm")

