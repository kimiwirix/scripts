#+ -----------------------------------------------------------------------------
#+ Master database of INDIV GC 

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

