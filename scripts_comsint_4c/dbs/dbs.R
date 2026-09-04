#+ MASTER DATABASES FROM 
#+ GC
#+ ODs
#+ QPCRs
#+ METADATA
#+ STRAINS IN COMSINTS


#+ -----------------------------------------------------------------------------
#+ Master database of GC 

library(readODS)
library(dplyr)

library(ggplot2)
library(pracma)
library(performance)
library(ggtext)


file<-"C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves.ods"
sheets<-ods_sheets("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves.ods")
ch<-sheets[grepl("^CH", sheets)]



t<-lapply(ch, function(ch_sheet){ #all sheets that start with CH in one tibble  
  read_ods(file, sheet = ch_sheet)
})%>%
  bind_rows()%>%
  filter(!is.na(`OD real`)) %>% #filters rows I put as separation between temps in ods 
  filter(!is.na(`fecha`))


#+ Remove due to shakers inconsistencies and in case of CH29 due to contamination 
#+ and in 07/02/26 4 biological replicates for ch29 were done again

t<-t%>% 
  filter(!(
    rep == 1 & Incubator =="B_2" |
      rep == 2 & Incubator =="B_2" |
      rep == 3 & Incubator =="B_2" ))%>%
  filter(!(Cepa == 'CH29' & rep %in% c(1,2,3,4,5,6,7)))



#+ export metadata info to upload in cluster shared_data
#+ ultimate filtered table with the useful data
 
write.table(t, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_filtered.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = FALSE)



#+ -----------------------------------------------------------------------------
#+ Master database of ODs from experiments 

library(readODS)
library(dplyr)


#Batches 0D
batch_1<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_1" )
batch_2<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_2" )
batch_3<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_3" )
batch_4<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_4" )
batch_5<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_5" )
batch_6<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_6" )
batch_7<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_7" )
batch_8<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_8" )
batch_9<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_9" )
batch_10<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_10" )
batch_11<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_11" )
batch_12<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods", sheet = "batch_12" )




#+une todos los batches, y elimina los timepoints 4,5,6 de los
#+primeros 2 batches 

b<-batch_1%>%
  bind_rows(batch_2)%>%
  bind_rows(batch_3)%>%
  bind_rows(batch_4)%>%
  bind_rows(batch_5)%>%
  bind_rows(batch_6)%>%
  bind_rows(batch_7)%>%
  bind_rows(batch_8)%>%
  bind_rows(batch_9)%>%
  bind_rows(batch_10)%>%
  bind_rows(batch_11)%>%
  bind_rows(batch_12)%>%
  select(!notes)%>%
  filter(!(timepoint %in% c(4, 5, 6)))



write.table(b, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ODs_db.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = TRUE)




#+ -----------------------------------------------------------------------------
#+ Master database of qPCRs from experiments 

library(readODS)
library(dplyr)


#+ carga el archivo y convierte los fragmentos de 16 /ul a fragmentos /ml 

qpcr<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/qPCR/qPCR.ods", sheet = "All" )

q<-qpcr%>%
  mutate_at(vars(`A-Fragmentos_16S_uL`, `B-Fragmentos_16S_uL`, `C-Fragmentos_16S_uL`), ~ . * 1000)%>%
  rename(`A_Fragmentos_16S_mL`=`A-Fragmentos_16S_uL`,
         `B_Fragmentos_16S_mL`=`B-Fragmentos_16S_uL`,
         `C_Fragmentos_16S_ml`=`C-Fragmentos_16S_uL`)



#+ correlation between the three replicates given by the three std curves
#+ are highly similar. thf, from now on we will use the mean measurement 
#+ calculated in b for further analysis

correlation <- q %>%
  summarise(
    cor_A_B = cor(A_Fragmentos_16S_mL, B_Fragmentos_16S_mL, method = 'pearson'),
    cor_A_C = cor(A_Fragmentos_16S_mL, C_Fragmentos_16S_ml, method = 'pearson'),
    cor_B_C = cor(B_Fragmentos_16S_mL, C_Fragmentos_16S_ml, method = 'pearson'),
    mean_cor = mean(c(cor_A_B, cor_A_C, cor_B_C), na.rm = TRUE)
  )
correlation


#+ Primero se hace la mean de las tres std curves. 
#+ Luego, las filas que tienen otas son esta nota: Se repitió. VER qpcr plots.
#+ Esas muestras se repitieron porque se comportaban raro. Las repeticiones 
#+ concuerdan con las otras réplicas por lo que se eliminaron las primeras 
#+ mediciones y se dejaron las repeticiones


b <- q %>%
  rowwise()%>%
  mutate(Fragmentos_16S_ml = mean(c(A_Fragmentos_16S_mL,B_Fragmentos_16S_mL,C_Fragmentos_16S_ml)))%>%
  select(!c(A_Fragmentos_16S_mL, B_Fragmentos_16S_mL, C_Fragmentos_16S_ml)) %>%
  filter(is.na(notas)) %>%
  select(!notas) 





write.table(b, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = TRUE)



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





#+ -----------------------------------------------------------------------------
#+ db of which strains are in which community taken from the CC data collection
#+ file

library(readODS)
library(dplyr)
library(tidyr)
library(reshape2)
library(tibble)

comm <- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_collection.ods", sheet = "comsints" )
communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")


p <- comm %>%
  column_to_rownames(var = "strain")%>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column(var = "community")%>%
  melt(variable.name="strain", value.name = "presence")%>%
  mutate(presence = ifelse(is.na (presence), 0, presence))%>%
  mutate(real_name = recode(strain,
                       "CH23"  = "Bacillus altitudinis",
                       "CH29"  = "Corynebacterium sp.",
                       "CH90"  = "Bacillus atrophaeus",
                       "CH99b" = "Staphylococcus arlettae",
                       "CH111" = "Bacillus thuringiensis",
                       "CH149a"= "Micrococcus luteus",
                       "CH154a"= "Staphylococcus shinii",
                       "CH161d"= "Bacillus infantis",
                       "CH447" = "Priestia megaterium",
                       "CH450" = "Metabacillus indicus")) %>%
  arrange(factor(community, levels = communities))


p
write.table( p,
             file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv" , 
             na = "NA",
             row.names = FALSE,
             col.names = TRUE,
             sep = "\t", 
             quote = TRUE)



#+ -----------------------------------------------------------------------------
#+ Ultimate color guide for strains and temperature colors.
#+ 


custom_colors <- c("Bacillus altitudinis"="#273a3eff", "Corynebacterium sp."="#08519cff",  "Staphylococcus arlettae"="#00e5eeff", "Bacillus thuringiensis"="#4682b4ff",  "Staphylococcus shinii"="#bdd7e7ff", "Bacillus atrophaeus"="#8c2424ff", "Micrococcus luteus"="#cd2626ff", "Bacillus infantis"="#ff0000ff", "Priestia megaterium"="#ef6d53ff", "Metabacillus indicus"="#fcae91ff" )
custom_colors <- c("CH23"="#273a3eff", "CH29"="#08519cff",  "CH99b"="#00e5eeff", "CH111"="#4682b4ff",  "CH154a"="#bdd7e7ff", "CH90"="#8c2424ff", "CH149a"="#cd2626ff", "CH161d"="#ff0000ff", "CH447"="#ef6d53ff", "CH450"="#fcae91ff" )
temp_colors <-  c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3")
strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Staphylococcus arlettae", "Bacillus thuringiensis", "Staphylococcus shinii", "Bacillus atrophaeus",  "Micrococcus luteus",  "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
real_names<-c('CH23'='<i>Bacillus altitudinis</i>', 'CH29'='<i>Corynebacterium sp.</i>', 'CH90'='<i>Bacillus atrophaeus</i>', 'CH99b'='<i>Staphylococcus arlettae</i>', 'CH111'='<i>Bacillus thuringiensis</i>', 'CH149a'='<i>Micrococcus luteus</i>', 'CH154a'='<i>Staphylococcus shinii</i>', 'CH161d'='<i>Bacillus infantis</i>',  'CH447'='<i>Priestia megaterium</i>', 'CH450'='<i>Metabacillus indicus</i>')





