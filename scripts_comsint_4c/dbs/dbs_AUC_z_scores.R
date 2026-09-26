#+ Master dbs from data in 
#+ C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/CC_data_od.ods 
#+ This dbs junta todos los batches en un solo df en ODs_db.R

library(dplyr)
library(pracma)

b <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ODs_db.tsv", header = TRUE,  sep='\t' )



#+ Primera parte añade el AUC de cada comunidad en cada temperatura a lo
#+ largo del tiempo pero quedan valores repetidos para cada tiempo, plt
#+ segunda parte elimina las filas repetidas dejando  fila por comsint por temp
#+ por réplica

b_AUC <- b%>%
  group_by(community, repbio, temp)%>%
  mutate(AUC=trapz(hrs,OD_real)) %>%
  ungroup() %>%
  group_by(community, repbio, temp) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop') 




#+ .tsv of the filtered data from individual growth curves 

t<-read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_filtered.tsv", 
              sep = '\t', 
              header = TRUE)



#AUC addition and eliminate filas que se repitan 
t_AUC<-t%>%
  group_by(Cepa, rep, temp)%>%
  mutate(AUC=trapz(hr,`OD.real`))%>%
  ungroup()%>%
  group_by(Cepa, rep, temp, Incubator) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop')%>%
  mutate(Cepa = recode(Cepa,
                       "CH23"  = "Bacillus altitudinis",
                       "CH29"  = "Corynebacterium sp.",
                       "CH90"  = "Bacillus atrophaeus",
                       "CH99b" = "Staphylococcus arlettae",
                       "CH111" = "Bacillus thuringiensis",
                       "CH149a"= "Micrococcus luteus",
                       "CH154a"= "Staphylococcus shinii",
                       "CH161d"= "Bacillus infantis",
                       "CH447" = "Priestia megaterium",
                       "CH450" = "Metabacillus indicus"))





#+ hacer un df que tenga los datos de AUC del df de individual y les pone 
#+ individual y del df de comunidad y les pone comunidad para que sea mas fácil 
#+ calcular la z score en los df t y b


a <- bind_rows(t_AUC %>%
    transmute(type = "individual",
      AUC = AUC),
    b_AUC %>%
    transmute(type = "community",
      AUC = AUC)) %>%
  mutate(AUC_z_score = scale(AUC))


t <- t_AUC %>%
  mutate(AUC_z_score = (AUC - mean(AUC))/ sd(AUC))
b <- b_AUC %>%
  mutate(AUC_z_score = (AUC - mean(AUC))/ sd(AUC))



write.table(t, file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/AUC_z_score_indivstrains.tsv", row.names=FALSE, sep="\t")
write.table(b, file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/AUC_z_score_comsint.tsv", row.names=FALSE, sep="\t")









