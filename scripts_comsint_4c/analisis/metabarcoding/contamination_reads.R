#+ Makes a final table with relative abundances, metadata and if the relative 
#+ abundances are or not contamination
#+ At the end it plots the level of contamination for each sample  

#+ it was a bit of trouble to include ensambles data 


library(dplyr)
library(textshape)
library(ggplot2)
library(reshape2)



#+ matched table

f <- read.csv(file = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/all_batch0_batches/feature-table-open_matched.tsv", sep = '\t', header = TRUE) %>%
  column_to_rownames(loc = "X.OTU.ID") 




#+ makes proportion table and transposes it and adds an extra column of the 
#+ labels of the community in format CC0000X

proportion_table<-apply(f, 2, function(x) x/sum(x)) 
p<-proportion_table %>%   
  t() %>% 
  as.data.frame()
p$label_final<-rownames(p)





#+ separation of proportion table into batches and ensambles 
#+ e filters the row that have from CC0001 to CC0032. There has to be a 
#+ separation because the ensamble information s not present in the metadata db, 
#+ since there is no temperature, no rep, etc etc

e <- p %>%
  filter(label_final %in% sprintf("CC%04d", 1:32)) %>%
  mutate(community = paste0('C', (1:32)))
b <- p %>%
  filter(!label_final %in% sprintf("CC%04d", 1:32))





#+ presence and metadata dbs 
#+ presence will tell me which strains should be in which community to identify 
#+ contamination 

s <- read.csv(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv", sep = '\t', header = TRUE)
m <- read.csv(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv", sep = '\t', header = TRUE) %>%
  select(!c(date, batch_extraction, batch_sequencing,batch_qpcr)) 





#+ Proportion table of batches united with the metadata based on the label name, 
#+ and leaves all other info

bm <- merge(b, m, by = "label_final") %>%
  melt(id = c('label_final', 'timepoint', 'community', 'techrep', 'temp', 'repbio', 'batch', 'label', 'hrs'), 
       variable.name = 'strain', value.name = 'rel_abd') %>%
  select(community, strain, label_final, rel_abd)

  



#+ cont_e: meltea la tabla de ensambles en tres columnas, la mergea con presence 
#+ db. 
#+ 
#+ cont: Mergea pm (metadata + prop table) con el presence df  (s), une el df 
#+ de cont_e y hace una nueva columna called contamination en la que: 
#+ 
#+ 
#+ Si la cepa no debe estar en la comunidad y no esta, plt se pone el nombre de la cepa
#+ Si la cepa debería estar en la comunidad y está, plt se pone el nombre de la cepa
#+ Cualquier otra cosa es contaminacion y se pone Contamination  
#+ Al final se eliminan los datos de la contaminación que tiene rel abd = 0

cont_e <- e %>%
  melt(id = c('label_final', 'community'), variable.name = 'strain', value.name = 'rel_abd') %>%
  merge(s, by = c('community','strain')) %>%
  select(label_final, rel_abd, strain, presence)


cont <- merge(bm, s, by = c('community','strain')) %>%
  select(label_final, rel_abd, strain, presence) %>%
  bind_rows(cont_e) %>%
  mutate(contamination=case_when(
    presence==0 & rel_abd==0 ~ 'no',
    presence==1 & rel_abd>=0 ~ 'no',
    presence==0 & rel_abd>0 ~ 'yes'
  )) 


cont $rel_abd <- as.numeric(cont$rel_abd)





#+ groups to show for each label (muestra) there is that relative abd of 
#+ contamination and that rel abd of not contamination 

c <- cont %>%
  select(label_final, rel_abd, contamination, strain) %>%
  group_by(label_final, contamination) %>%
  summarise(total_frequency = sum(rel_abd), .groups = "drop")



p <- ggplot(data = c, aes(x = label_final, y = total_frequency, fill = contamination))+
  geom_bar(position = "stack", stat = "identity")+
  labs(title = "Percentage of contamination in samples",
       y = "Frequency", x = "Sample", fill="Contamination") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 3, angle = 90, hjust = 1)) 
  

p
ggsave(p,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/all_batch0_batches/contamination/contamination_reads.png" ,
       bg="white",  width = 58.7, height = 15, units = "cm")

