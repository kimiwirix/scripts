#grafica barplots con tablas de frequecias ya sin contaminacion
library(readODS)
library(reshape2)
library(ggplot2)
library(patchwork)
library(remotes)
library(sjstats)
library(dplyr)
library(tibble)
library(tidyr)


#cambiar dependiendo el archivo 
frequency_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/f_clean.tsv', sep = '\t', header = TRUE, row.names = "row.names")

proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 
proportion_table<-proportion_table %>%   
  t() %>% 
  as.data.frame()

proportion_table$label<-rownames(proportion_table) #pone como primera columna los rownames para mergear con las otras tablas
p<-proportion_table%>%
  rename("Pseudomonas sp.2"="ST00101", "Agrobacterium sp."="ST00154", "Pseudomonas sp.1"="NS_042g_27F", "Arthrobacter sp."="ST00060", "Bacillus sp.1"="ST00046", "Paenibacillus sp."="ST00143", "Bacillus sp.2"="NS_164C_27F", "Variovorax sp."="NS_110C_1_27F", "Rhodococcus sp."="ST00094", "Mycobacterium sp."="ST00109")

#METADATA 
metadata_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/metadata_clean.tsv', sep = '\t', header = TRUE)
metadata_table<-metadata_table%>%
  select(!day)
sort(metadata_table$label)==sort(colnames(frequency_table))


proportion_metadata<-merge(p, metadata_table, by = "label") #mergea metadata con proportion table por el label name 

communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
custom_colors <- c("Bacillus sp.1"="#25383C","Agrobacterium sp."="#08519C", "Pseudomonas sp.2"="#00E5EE", "Mycobacterium sp."="#4682B4", "Pseudomonas sp.1"="#BDD7E7", "Paenibacillus sp."="#8B2323", "Bacillus sp.2"="#CD2626", "Rhodococcus sp."="#FF3030" ,"Variovorax sp."="#EE6A50","Arthrobacter sp."="#FCAE91" )
strains <- c("Bacillus sp.1","Agrobacterium sp.", "Pseudomonas sp.2", "Mycobacterium sp.", "Pseudomonas sp.1", "Paenibacillus sp.", "Bacillus sp.2", "Rhodococcus sp." ,"Variovorax sp.","Arthrobacter sp.")
strains_labels_italic <- setNames(lapply(strains, function(x) bquote(italic(.(x)))), strains)

melt_data <- melt(proportion_metadata, id = c('label', 'hrs', 'community','techrep','temp','exp'), variable.name = 'strain')#reorganiza tabla por strains 

#df_NAs duplica las filas que tienen NA de temp, y a una le pone 28 y a otra 32 para que aparezcan dentro del plot en tiempo 0
df_NAs <- melt_data %>%
  filter(is.na(temp))%>%
  uncount(2) %>%
  mutate(temp = rep(c("28", "32"), length.out = n())) 

melt_data <- melt_data %>% #de la tabla por strain quita día 0-temp NA, y agrupa por comunidad y temp los values y saca el promedio de values por cada temp en cada community
  filter(temp != "NA")%>%
  rbind(df_NAs)%>%
  group_by(strain ,community , temp, hrs)%>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")%>% # remove if dont want to plot averages
  as.data.frame()


melt_data$community <- factor(melt_data$community, levels = communities) # Custom order



#"R7", "R8", "R9", "R10", "R11", "R12"
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
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/strains in comsint/avg_NS_barplot_allbatches_99ident_pt2.png" ,
       bg="white",  width = 30, height = 14, units = "cm")



#export metadata info to upload in cluster shared_data
write.table(metadata, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/metadata_allbatches.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = FALSE)
