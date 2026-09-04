#+ script para hacer las graficas (barplots) de los batches enviados a secuenciar
#+ 32 comunidades, 3 temperaturas, dos replicas (para comsints 3,24,27 son 3 reps)
#+ y 4 tiempos contando el T0 de los ensambles (hrs: 0, 9, 12, 24)

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
#+ arrojando wierd values, this takes the whole .tsv from qiime and only leaves the 
#+ reads that matched to the strains 

f <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/matched.tsv", sep = "\t",
         header = TRUE) 

m <- f %>%
  filter(X.OTU.ID %in% strain_ids) %>%
  column_to_rownames(var = "X.OTU.ID")



#+ makes proportion table and transposes it and adds an extra column of the 
#+ labels of the community in format CC0000X

proportion_table<-apply(m, 2, function(x) x/sum(x)) 
p<-proportion_table %>%   
  t() %>% 
  as.data.frame()
p$label_final<-rownames(p)




#+ METADATA is already a nonchanging db

m <- read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv")




#+ Proportion table and metadata together based on the label name, and leaves all other info

pm <- merge(p, m, by = "label_final")%>%    
  melt(id = c('label_final', 'timepoint', 'community', 'techrep', 'temp', 'repbio', 'batch', 'label', 'hrs'), 
       variable.name = 'strain', value.name = 'rel_abd') %>%
  select(!c("label", "techrep", "batch"))



#PRESENCE df que nos dice las strains que deben estar en que comsint, ya es una unchanging db 

s <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv",
                       header = TRUE)


#+ Mergea pm (metadata + prop table) con el presence df y hace una nueva columna 
#+ called contamination:
#+ Si la cepa no debe estar en la comunidad y no esta, plt se pone el nombre de la cepa
#+ Si la cepa debería estar en la comunidad y está, plt se pone el nombre de la cepa
#+ Cualquier otra cosa es contaminacion y se pone Contamination  
#+ Al final se eliminan los datos de la contaminación que tiene rel abd = 0

c<- merge(pm, s, by = c('community','strain')) %>%  
    mutate(contamination=case_when(
    presence==0 & rel_abd==0 ~ real_name,
    presence==1 & rel_abd>0 ~ real_name,
    TRUE  ~ 'Contamination'
  )) %>% 
  filter(!(contamination=="Contamination" & rel_abd ==0))




#+ Aqui se agregan los datos de los ensambles. 
#+ Pero hay un problema, los ensambles no tienen temp ni repbio, entonces 
#+ en temp_rep se recopiln todas las combinaciones de comunidad+temp+repbio 
#+ que son diferentes y eso se agrega con left join a los datos de los ensambles 
#+ para que se repitan las mediciones de cada cepa tres veces por las temps
#+ y dos veces (o 3) por las repbios  

temp_rep <- c %>%
  distinct(community, temp, repbio)


e <-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ensambles.tsv',
                    sep = "\t", header = TRUE) %>%
  rename (label_final = community_label) %>%
  mutate (hrs = 0) %>%
  select(community, strain, label_final, timepoint,rel_abd, presence, real_name, contamination, hrs) %>%
  left_join(
    temp_rep,
    by = "community")



#+ All. joins both df 

a <- c %>%
  full_join(e)



#+ INFO 

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
custom_colors <- c("Bacillus altitudinis"="#273a3eff", "Corynebacterium sp."="#08519cff",  "Staphylococcus arlettae"="#00e5eeff", "Bacillus thuringiensis"="#4682b4ff",  "Staphylococcus shinii"="#bdd7e7ff", "Bacillus atrophaeus"="#8c2424ff", "Micrococcus luteus"="#cd2626ff", "Bacillus infantis"="#ff0000ff", "Priestia megaterium"="#ef6d53ff", "Metabacillus indicus"="#fcae91ff" )
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Staphylococcus arlettae", "Bacillus thuringiensis", "Staphylococcus shinii", "Bacillus atrophaeus",  "Micrococcus luteus",  "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
italic <- setNames(lapply(strains, function(x) bquote(italic(.(x)))), strains)



#+ New labels son para modificar los labels del facet grid 

new_labels_rep <- c("1" = "Replicate 1", "2" = "Replicate 2", "3" = "Replicate 3")
new_labels_temp <- c("30" = "30°C", "37" = "37°C", "42" = "42°C")




#+ for loop para sacar plots de todas las comunidades y guardarlas en directory 

for (comsint in communities) {
  
  pt <- ggplot(data = a %>%
           filter(community==comsint), aes(x = as.factor(hrs), y = rel_abd, fill =contamination, group=strain))+ 
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(repbio ~temp, labeller = labeller(repbio = new_labels, temp=new_labels_temp)) +
    scale_fill_manual(values = c(custom_colors, Contamination="#FFED29"),
                      labels = italic) + 
    labs(title = paste0( comsint, " composition"),
         y = "Relative abundance", x = "Time (hrs)", fill="Strain") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  
  ggsave(plot = pt,
         filename = file.path(paste0("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/barplots/", comsint, ".png")),
         bg="white",  width = 30, height = 17, units = "cm")   
  
}




