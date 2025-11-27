#cuanta contaminación hay en la tabla matched?
#compares matched table frequencies column vs matched table without contamination column both are columns in last df 


#CONCLUSIONS: f is the df without contaminated samples (cont>20%) and without noise abundances (<50)


library(readODS)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(tidyverse)


#FREQUENCY DATA
frequency_table<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches_99ident/feature-table-open-all-99ident.ods", sheet = "matched")
frequency_table<-frequency_table %>%
  tibble::column_to_rownames(var="#OTU ID") %>%
  as.matrix()

colnames(frequency_table) <- ifelse(grepl("^N5", colnames(frequency_table)), sub("5", "S", colnames(frequency_table)), colnames(frequency_table)) #cambia el primer 5 a S de solo los que empiezan con N5




#METADATA 
NS1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")
NS2<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS2.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")
NS3<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS3.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")
NS4<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS4.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp","temp", "hrs")


NS<-rbind(NS1,NS2,NS3,NS4)

#METADATA SPECIFICS

#NS that we had to use techrep B since A failed
labels_B<- c("NS00037B", "NS00038B", "NS00039B", "NS00040B", "NS00041B", "NS00042B",
             "NS00043B", "NS00044B", "NS00045B", "NS00046B", "NS00047B", "NS00048B",
             "NS00079B", "NS00080B", "NS00081B", "NS00082B", "NS00083B", "NS00084B",
             "NS00085B", "NS00086B", "NS00088B", "NS00089B", "NS00090B", "NS00165B",
             "NS00169B", "NS00171B", "NS00173B")
labels_A <- gsub("B$", "A", labels_B)


NS<-NS%>%
  filter(techrep=="A" | (techrep=="B" & label %in% labels_B)) %>%
  filter(!label %in% labels_A)%>%
  filter(!label == "NS00087A") #lost sample:(



#COMSINTS INFO  (which strain is in which community)
NS1_comsints<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet="syncoms")%>%
  as.data.frame()

NS2_comsints<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet="syncoms")%>%
  as.data.frame()




#df transformation: junta info comsints + que cepas estan en cada comsint
NS_comsints_metadata<-NS2_comsints%>%
  left_join(NS1_comsints, by = "strain")%>%
  tibble::column_to_rownames("strain")%>%
  t()%>%
  as.data.frame()%>%
  tibble::rownames_to_column(var="community")%>%
  full_join(NS, by = "community")%>%
  select(!"techrep")%>%
  rename("NS_042g_27F"="ST00042", "NS_164C_27F"="ST00164","NS_110C_1_27F"="ST00110")


NS_comsints_metadata_melted<-melt(NS_comsints_metadata, id=c("label", "exp", "community", "temp", "hrs"), variable.name = "strain", value.name = "presence" )


frequency_table<-t(frequency_table)%>%
  as.data.frame()%>%
  tibble::rownames_to_column(var="label")

frequency_table_melted<-melt(frequency_table, id = "label", variable.name = "strain", value.name = "abundances")



#melt both dataframes with only 3 columns: label, strain and value (relative freq and presence) and the merge 
merged_df<-merge(frequency_table_melted, NS_comsints_metadata_melted, by = c("label", "strain"))






#relative frequencies that are not supposed to be there are converted to 0s and then multiplyed by abundances to know how community would be ideally without cont 
#agrega columna que pone yes a las rows que son contaminacioon
df<-merged_df%>%  
  mutate(presence=replace_na(presence, 0))%>%
  mutate(wo_contamination=abundances*presence)
  
df<-df%>%
  mutate(contamination=case_when(df$abundances != df$wo_contamination ~ "YES",
                                 TRUE ~ "NO"))%>%
  group_by(label)%>%
  mutate(rel_freq= abundances/sum(abundances))%>%
  ungroup()





#agrega col contaminated_rel_freq que es suma de las proportions de las que si son contaminacion de cada NS y si son mayores a 20%  quita las muestras 
filtered <- df %>%
  group_by(label) %>%
  mutate(contaminated_rel_freq = sum(rel_freq[contamination == "YES"], na.rm = TRUE))%>%
  filter(!(contaminated_rel_freq>0.20))%>%
  ungroup()

#only label and contamination, sums all yes and no contamination for each label and plots  
filtered %>%
  group_by(label, contamination) %>%
  summarise(tot = sum(abundances),
            .groups = "drop") %>%
  ggplot( aes(x = label, y = tot)) +
  geom_bar(aes(fill = contamination), stat = "identity", position = "fill",
           width = 1) +
  theme(axis.text.x = element_text(angle = 90))



####prepare for ancom:
#lista de muestras que voy a quitar para quitarlas tanto de freq como de metadata
removed_labels <- setdiff(df$label, filtered$label)
removed_labels_str <- paste0("c(\"", paste(removed_labels, collapse = "\", \""), "\")")

#direct pase in ancom
cat(removed_labels_str)



###Removing low abundances (abundances less than 50 counts)
filtered_2 <- filtered %>%
  filter(!(rel_freq<5.180757e-04 & contamination=="YES"))

removed_strains <- anti_join(filtered, filtered_2, by = c("label", "strain"))%>%
  select(c("label","strain","abundances"))%>%
  mutate(abundances=0)





#FINAL FREQUENCY TABLE
#elimina los removed_labels y pone un cero en las freuqnecias que son contaminacion y son menores de 50 counts 

f_clean<-frequency_table%>%
  as.data.frame()%>%
  select(!all_of(removed_labels))%>%
  tibble::rownames_to_column(var = "strain")%>%
  pivot_longer(!strain, names_to = "label", values_to = "abundance")%>%
  left_join(removed_strains, by = c("label","strain"))%>%
  mutate(abundance = if_else(!is.na(abundances), abundances, abundance)) %>%
  select(-abundances)%>%
  pivot_wider(names_from = label, values_from = abundance) %>%
  column_to_rownames(var = "strain")





#f is the df without contaminated samples (cont>20%) and without noise abundances (<50)



#METADATA 
NS1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "day", "label", "techrep", "exp", "temp", "hrs")
NS2<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS2.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "day", "label", "techrep", "exp", "temp", "hrs")
NS3<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS3.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "day", "label", "techrep", "exp", "temp", "hrs")
NS4<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS4.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "day", "label", "techrep", "exp", "temp", "hrs")
NS<-rbind(NS1,NS2,NS3,NS4)


#TABLE AND METADATA MERGED

#NS that we had to use techrep B since A failed
labels_B<- c("NS00037B", "NS00038B", "NS00039B", "NS00040B", "NS00041B", "NS00042B",
             "NS00043B", "NS00044B", "NS00045B", "NS00046B", "NS00047B", "NS00048B",
             "NS00079B", "NS00080B", "NS00081B", "NS00082B", "NS00083B", "NS00084B",
             "NS00085B", "NS00086B", "NS00088B", "NS00089B", "NS00090B", "NS00165B",
             "NS00169B", "NS00171B", "NS00173B")
labels_A <- gsub("B$", "A", labels_B)


metadata_clean<-NS%>%
  filter(techrep=="A" | (techrep=="B" & label %in% labels_B)) %>%
  filter(!label %in% labels_A)%>%
  filter(!label == "NS00087A")%>% #lost sample:(
  filter(!label %in% removed_labels) #keep if want to remove labels with high contamination 



#comprobacion
sort(metadata_clean$label)==sort(colnames(f_clean))



write.table(metadata_clean,file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/metadata_clean.tsv" , na = "NA", append = TRUE, col.names = TRUE, row.names = FALSE, sep = "\t", quote = TRUE)
write.table(f_clean%>%tibble::rownames_to_column(var = "row.names"),file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/f_clean.tsv" , na = "NA", append = TRUE, row.names = FALSE, col.names=TRUE,sep = "\t", quote = TRUE)
write.table(NS_comsints_metadata_melted,file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/presence_strains_in_comsints.tsv" , na = "NA", append = TRUE, col.names = TRUE, row.names = FALSE, sep = "\t", quote = TRUE)
