#Calculates from all the comsints the percentage of frequencies corresponding to unmatched and of matched 
#the calculation was done for 99 perc indetity and 97 by changing the files in the frequency tables 

#CONCLUSIONS: with more perc identity the ratio of matches is less by 6%, we loose few information and is more strict on detecting the 10 species 

library(readODS)
library(dplyr)
library(tidyverse)

#File for 97 percent all batches: "C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches/feature-table-open-all-allbatches.ods" 
frequency_table_matched<-read_ods("C:/Users/natal/Downloads/PRUEBA/feature-table-prueba.ods", sheet = "matched") %>%
  column_to_rownames(var = "#OTU ID")%>%
  as.data.frame()

frequency_table_unmatched<-read_ods("C:/Users/natal/Downloads/PRUEBA/feature-table-prueba.ods", sheet = "unmatched") %>%
  column_to_rownames(var = "#OTU ID")%>%
  as.data.frame()

frequency_table_together<-read_ods("C:/Users/natal/Downloads/PRUEBA/feature-table-prueba.ods", sheet = "feature-table-open-all") %>%
  column_to_rownames(var = "# Constructed from biom file")%>%
  janitor::row_to_names(row_number =1)%>% 
  as.data.frame()%>%
  mutate(across(everything(), ~ as.numeric(unlist(.))))


percent_matched<-colSums(frequency_table_matched)/colSums(frequency_table_together)
percent_unmatched<-colSums(frequency_table_unmatched)/colSums(frequency_table_together)


mean(percent_matched)
min(percent_matched)
max(percent_matched)


plot(percent_matched, ylim = c(0,1))

