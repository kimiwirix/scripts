
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

