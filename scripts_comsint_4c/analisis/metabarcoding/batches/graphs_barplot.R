#+ script para hacer las graficas de los ensambles enviados a secuenciar en batch 0
#+ no hacer limpia de contaminacion en ensambles porque a aprtir de eso van a crecer las cepas 

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

#+ Before downloading table, remove the # Constructed from biom file line in 
#+ Notepad ++. 

#+ I dont know why read.table was not working for reading the .tsv, and was 
#+ arrojando wierd values 

f <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/matched.tsv", sep = "\t",
         header = TRUE) 

m <- f %>%
  filter(X.OTU.ID %in% strain_ids) %>%
  column_to_rownames(var = "X.OTU.ID")



#+ makes proportion table
proportion_table<-apply(m, 2, function(x) x/sum(x)) 
p<-proportion_table %>%   
  t() %>% 
  as.data.frame()
p$label_final<-rownames(p)


p

#METADATA 
m <- read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv")



#proportion and metadata together
p_m <- merge(p, m, by = "label_final")%>%   #mergea metadata con proportion table por el label name 
  melt(id = c('label_final', 'timepoint', 'community', 'techrep', 'temp', 'repbio', 'batch', 'label', 'hrs'), 
       variable.name = 'strain', value.name = 'rel_abd')


#CHECAR QUE ALS QUE ESTÁN SI DEBEN DE ESTAR 
#se guardó al final como presence 
s <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv",
                       header = TRUE)


#reorganiza tabla por strains y su value de relative abundance 
df<- merge(p_m, s, by = c('community','strain'))%>% #mergea metadata con proportion table por el label name 
    mutate(contamination=case_when(
    presence==0 & rel_abd==0 ~ real_name,
    presence==1 & rel_abd>0 ~ real_name,
    TRUE  ~ 'Contamination'
  )) %>% 
  filter(!(contamination=="Contamination" & rel_abd ==0))



communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
custom_colors <- c("Bacillus altitudinis"="#273a3eff", "Corynebacterium sp."="#08519cff",  "Staphylococcus arlettae"="#00e5eeff", "Bacillus thuringiensis"="#4682b4ff",  "Staphylococcus shinii"="#bdd7e7ff", "Bacillus atrophaeus"="#8c2424ff", "Micrococcus luteus"="#cd2626ff", "Bacillus infantis"="#ff0000ff", "Priestia megaterium"="#ef6d53ff", "Metabacillus indicus"="#fcae91ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Staphylococcus arlettae", "Bacillus thuringiensis", "Staphylococcus shinii", "Bacillus atrophaeus",  "Micrococcus luteus",  "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(lapply(strains, function(x) bquote(italic(.(x)))), strains)




df$community <- factor(df$community, levels = communities) # Custom order

new_labels_rep <- c("1" = "Replicate 1", "2" = "Replicate 2", "3" = "Replicate 3")
new_labels_temp <- c("30" = "30°C", "37" = "37°C", "42" = "42°C")
comsint<-"kk"

ggplot(data = df %>%
               filter(community=="C25"), aes(x = as.factor(hrs), y = rel_abd, fill =contamination, group=strain))+ #just value if dont want to plot averages
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(repbio ~temp, labeller = labeller(repbio = new_labels, temp=new_labels_temp)) +
  scale_fill_manual(values = c(custom_colors, Contamination="#FFED29"),
                                        labels = italic) + 
  labs(title = paste0( comsint, " composition"),
       y = "Relative abundance", x = "Time (hrs)", fill="Strain") + 
  theme(plot.title = element_text(hjust = 0.5))

  
  
  plot<-ggplot(data = melt_data%>%
                 filter(community %in% c("R7", "R8", "R9", "R10", "R11", "R12")), aes(x = hrs, y = mean_value, fill = strain))+ #just value if dont want to plot averages
    geom_bar(position = "stack", stat = "identity")+
    scale_fill_manual(values = custom_colors,
                      labels = strains_labels_italic,
                      breaks = strains)+
    facet_wrap(~community+temp, ncol = 6)+ #Add +exp if we dont want to plot averages 
    scale_x_continuous(breaks = c(0, 24, 48,72))+
    labs(title = "Community composition through time and temperature",
         y = "Abundance", x = "Time (hrs)", fill="Strain") + 
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  

plot


ggsave(plot,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/ensamble_composition.png" ,
       bg="white",  width = 30, height = 14, units = "cm")





