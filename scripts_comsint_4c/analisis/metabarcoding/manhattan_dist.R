#+ DISCLAIMER: the unmatched table that was used here was a cropped version from
#+ the real data in feature-table-open. It was cropped manually (in NOTEPAD++)
#+ removing the last thousands of columns containing just 0s. 
#+ CROPPED: unmatched_cropped.tsv
#+ NON-CROPPED: unmatched.tsv
#+ 
#+ 
#+ 
#+ I have my feature-table.tsv divided into what matched to the references and 
#+ what didnt match (unmatched). I want to compare the pattern of presence in the 
#+ matched table for each strain to the pattern of the unmatched using manhattan
#+ distances. 
#+ 
#+ I need a df that tells me for every sample if a strain should be present or not 
#+ and that I will compare rowwise to the patterns n the unmatched and if manhattan 
#+ dist == 0 then it is probable that that is one strain that failed to match to
#+ the refs. 


library(dplyr)
library(tidyr)
library(textshape)
library(abdiv)
library(tibble)



#+ Cargar datos de ensambles 
#+ Cragar datos de metadata, para hacer usar las manhattan distances solo lo voy 
#+ a hacer para el primer tiempo (TP1.5). Voy a comparar la presencia ausencia de
#+ cada cepa en el ensamble vs la presencia o ausencia (0s y 1s) de la unmatched y voy a separar 
#+ los vecotres que den manhatt==0, lo voy a hacer de todas las comunidades al 
#+ mismo timpo pero solo de el primer tp porque en los siguientes tps si la cepa 
#+ no esta presente puede ser porque se murió y no tiene nada que ver con un 
#+ error de identificacion contra la referencia 

#p <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv", header = TRUE) %>%
#  select(!real_name)

m <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv", header = TRUE) %>%
  filter (timepoint==1.5) %>%
  select(community, label_final)
  

e <- read.csv(file = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/feature-table-open-ensambles-nonchimeric.tsv", header = TRUE, sep = "\t") %>%
  filter(startsWith(strain, "CH")) %>%
  select(1:33) %>%
  pivot_longer(cols= -strain, names_to = "community", values_to = "presence") %>%
  mutate(presence =  ifelse( presence > 0, 1, 0)) %>%
  mutate(community = paste0("C", as.integer(sub("CC", "", community))))
  





#+ dataframe que organiiza presencia y ausencia teorética para comparar por cada 
#+ cepa (cada row) con las unmatched

a <- m %>%
  distinct() %>%
  left_join(e, by="community") %>%
  select(!community) %>%
  pivot_wider(names_from = "label_final", values_from = "presence") %>%
  column_to_rownames(var = "strain")




#+ Carga las unmatched y elimina las filas que tengan menos de 100 reads 
#+ (límite arbitrario) y cambia los numeros > 0 por 1s 

unmatched <- read.csv(file = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/unmatched_cropped.tsv", header = TRUE, sep = "\t") %>%
  column_to_rownames(var = "X.OTU.ID")

u <- unmatched %>%
  select(colnames(a)) %>%
  filter (!rowSums(.) < 100) %>%
  mutate(across(everything(), ~ ifelse( . > 0, 1, 0))) 



#+ compare every a row to every u row and calculate the manhattan distance
#+ save the u id and the a id and the manhattan value in a dataframe 


results <- data.frame()

for (i in 1:nrow(a)) {
  
  x <- a[i, ]
  for (j in 1:nrow(u)) {
    
    y <- u[j, ]
    md <- manhattan(x, y)
      
      results <- rbind(
        results,
        data.frame(
          strain_a = rownames(a[i,]),
          strain_u = rownames(u[j,]),
          mand = md
        ))}}


r <- arrange(results, mand)


write.table(r, file = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/manhattan_dist_unmatched.tsv", row.names=FALSE, sep="\t")




