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


#el archivo matched, para el caso de ensambles no hay que quitarle la contaminacion ni filtrar
frequency_table<-read_ods('C:/Users/natal/Downloads/PRUEBA/feature-table-prueba.ods', sheet = 'matched')%>%
  column_to_rownames(var='#OTU ID')


#makes proportion table
proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 
proportion_table<-proportion_table %>%   
  t() %>% 
  as.data.frame()
proportion_table$community_label<-rownames(proportion_table)


#pone como primera columna los rownames para mergear con las otras tablas
p<-proportion_table%>%
  rename("Bacillus altitudinis"="CH23", "Corynebacterium sp."="CH29", "Bacillus atrophaeus"="CH90", "Staphylococcus arlettae"="CH99b", "Bacillus thuringiensis"="CH111", 
         "Micrococcus luteus"="CH149a", "Staphylococcus shinii"="CH154a", "Bacillus infantis"="CH161d", "Priestia megaterium"="CH447", "Metabacillus indicus"="CH450")

#METADATA 
m<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_data_collection.ods", sheet = "ensamble")%>%
  filter(repbio=="A")%>%
  select(!c(notes, repbio, date))


#proportion and metadata together
p_m<-merge(p, m, by = "community_label") #mergea metadata con proportion table por el label name 


communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
custom_colors <- c("Bacillus altitudinis"="#ff0000ff", "Corynebacterium sp."="#cd2626ff", "Bacillus atrophaeus"="#fcae91ff", "Staphylococcus arlettae"="#4682b4ff", "Bacillus thuringiensis"="#8c2424ff",
                   "Micrococcus luteus"="#00e5eeff", "Staphylococcus shinii"="#ef6d53ff", "Bacillus infantis"="#08519cff", "Priestia megaterium"="#273a3eff", "Metabacillus indicus"="#bdd7e7ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Bacillus atrophaeus", "Staphylococcus arlettae", "Bacillus thuringiensis","Micrococcus luteus", "Staphylococcus shinii", "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(lapply(strains, function(x) bquote(italic(.(x)))), strains)




#reorganiza tabla por strains y su value de relative abundance 
df <- melt(p_m, id = c('community_label', 'timepoint', 'community'), variable.name = 'strain', value.name = 'rel_abd')
df$community <- factor(df$community, levels = communities) # Custom order




plot<-ggplot(data = df, aes(x = community, y = rel_abd, fill = strain))+ #just value if dont want to plot averages
  geom_bar(position = "stack", stat = "identity")+
  labs(title = "Ensamble composition",
       y = "Abundance", x = "Community", fill="Strain") + 
  scale_fill_manual(values = custom_colors,
                    labels = italic)+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))+
  theme(plot.title = element_text(hjust = 0.5))



plot
ggsave(plot,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/ensamble" ,
       bg="white",  width = 30, height = 14, units = "cm")





