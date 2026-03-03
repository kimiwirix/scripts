#script para hacer las graficas de los ensambles enviados a secuenciar en batch 0
#no hacer limpia de contaminacion en ensambles porque a aprtir de eso van a crecer las cepas 

library(readODS)
library(reshape2)
library(ggplot2)
library(patchwork)
library(remotes)
library(sjstats)
library(dplyr)
library(tibble)
library(tidyr)



strain_ids<-c("CH111","CH90","CH161d", "CH149a","CH29","CH99b","CH154a","CH23","CH447", "CH450")  
  
matched<-read.table(file = 'C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/feature-table-open-ensambles-nonchimeric.tsv',
                            sep = "\t", header = TRUE)%>%
  filter(strain %in% strain_ids)%>%
  column_to_rownames(var = 'strain')



#makes proportion table
proportion_table<-apply(matched, 2, function(x) x/sum(x)) 
p<-proportion_table %>%   
  t() %>% 
  as.data.frame()
p$community_label<-rownames(p)





#METADATA 
m<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_data_collection.ods", sheet = "ensamble")%>%
  filter(repbio=="A")%>%
  select(!c(notes, repbio, date))


#proportion and metadata together
p_m<-merge(p, m, by = "community_label")%>%   #mergea metadata con proportion table por el label name 
  melt(id = c('community_label', 'timepoint', 'community'), variable.name = 'strain', value.name = 'rel_abd')


#CHECAR QUE ALS QUE ESTÁN SI DEBEN DE ESTAR 
ok<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_data_collection.ods", sheet = "comsints")%>%
  column_to_rownames(var = "strain")%>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column(var = "community") %>%
  melt(variable.name="strain", value.name = "presence")%>%
  mutate(presence = ifelse(is.na (presence), 0, presence))
  




#reorganiza tabla por strains y su value de relative abundance 
df<- merge(p_m, ok, by = c('community','strain'))%>% #mergea metadata con proportion table por el label name 
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
                         "CH450" = "Metabacillus indicus"))%>%
  mutate(contamination=case_when(
    presence==0 & rel_abd==0 ~ strain,
    presence==1 & rel_abd>0 ~ strain,
    TRUE  ~ 'Contamination'
  ))



communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
custom_colors <- c("Bacillus altitudinis"="#ff0000ff", "Corynebacterium sp."="#cd2626ff", "Bacillus atrophaeus"="#fcae91ff", "Staphylococcus arlettae"="#4682b4ff", "Bacillus thuringiensis"="#8c2424ff",
                   "Micrococcus luteus"="#00e5eeff", "Staphylococcus shinii"="#ef6d53ff", "Bacillus infantis"="#08519cff", "Priestia megaterium"="#273a3eff", "Metabacillus indicus"="#bdd7e7ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Bacillus atrophaeus", "Staphylococcus arlettae", "Bacillus thuringiensis","Micrococcus luteus", "Staphylococcus shinii", "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(lapply(strains, function(x) bquote(italic(.(x)))), strains)




df$community <- factor(df$community, levels = communities) # Custom order




plot<-ggplot(data = df, aes(x = community, y = rel_abd, fill =contamination, group=strain))+ #just value if dont want to plot averages
  geom_bar(position = "stack", stat = "identity")+
  labs(title = "Ensamble composition",
       y = "Abundance", x = "Community", fill="Strain") + 
  scale_fill_manual(values = c(custom_colors, Contamination="black"),
                    labels = italic)+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))+
  theme(plot.title = element_text(hjust = 0.5))


plot
ggsave(plot,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/ensamble_composition.png" ,
       bg="white",  width = 30, height = 14, units = "cm")





