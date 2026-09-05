#+ -----------------------------------------------------------------------------
#+ Master database of metadata taken from DNA_extr_batches.ods in sequencing window
#+ since that window has the latest labels and the samples sent to sequencing
#+ from wich the qPCRs were done 


library(readODS)
library(dplyr)

seq<- read_ods(path ="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/DNA_extr_batches.ods", sheet = "sequencing" )

b<-seq%>%
  select(community, techrep, temp, timepoint, repbio, label, label_final, batch)%>%
  mutate(hrs= case_when(
    timepoint == 1.5 ~ 9,
    timepoint == 2 ~ 12,
    timepoint == 3 ~ 24
  ))%>%
  mutate(community = sub("^C0", "C", community))

write.table(b, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = TRUE)
