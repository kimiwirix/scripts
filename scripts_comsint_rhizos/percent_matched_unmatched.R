#Calculates from all the comsints the percentage of frequencies corresponding to unmatched and of matched 


library(readODS)
library(dplyr)
library(tidyverse)


frequency_table_matched<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches/feature-table-open-all-allbatches.ods", sheet = "matched") %>%
  column_to_rownames(var = "#OTU ID")%>%
  as.data.frame()

frequency_table_unmatched<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches/feature-table-open-all-allbatches.ods", sheet = "unmatched") %>%
  column_to_rownames(var = "#OTU ID")%>%
  as.data.frame()

frequency_table_together<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches/feature-table-open-all-allbatches.ods", sheet = "feature-table-open-all") %>%
  column_to_rownames(var = "# Constructed from biom file")%>%
  janitor::row_to_names(row_number =1)%>% 
  as.data.frame()%>%
  mutate(across(everything(), ~ as.numeric(unlist(.))))


percent_matched<-colSums(frequency_table_matched)/colSums(frequency_table_together)
percent_unmatched<-colSums(frequency_table_unmatched)/colSums(frequency_table_together)



