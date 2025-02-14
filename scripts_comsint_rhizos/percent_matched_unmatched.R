#Calculates from all the comsints the percentage of frequencies corresponding to unmatched and of matched 


library(readODS)
library(dplyr)
library(tidyverse)


frequency_table_matched<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches_99ident/feature-table-open-all-99ident.ods", sheet = "matched") %>%
  column_to_rownames(var = "#OTU ID")%>%
  as.data.frame()

frequency_table_unmatched<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches_99ident/feature-table-open-all-99ident.ods", sheet = "unmatched") %>%
  column_to_rownames(var = "#OTU ID")%>%
  as.data.frame()

frequency_table_together<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches_99ident/feature-table-open-all-99ident.ods", sheet = "feature-table-open-all") %>%
  column_to_rownames(var = "# Constructed from biom file")%>%
  janitor::row_to_names(row_number =1)%>% 
  as.data.frame()%>%
  mutate(across(everything(), ~ as.numeric(unlist(.))))


percent_matched<-colSums(frequency_table_matched)/colSums(frequency_table_together)
percent_unmatched<-colSums(frequency_table_unmatched)/colSums(frequency_table_together)

#id_97_matched<-percent_matched
#id_97_unmatched<-percent_unmatched

id_99_matched<-percent_matched
id_99_unmatched<-percent_unmatched

comparison<-data.frame(matched_97=id_97_matched,
               matched_99=id_99_matched,
               unmatched_97=id_97_unmatched,
               unmatched_99=id_99_unmatched)

colMeans(comparison)
apply(comparison, 2, min)
apply(comparison, 2, max)
