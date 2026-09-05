
#+ -----------------------------------------------------------------------------
#+ Master database of qPCRs from experiments, incorporates metadata, and ensamble 
#+ information in the final table. And adjusts volumes to Frag/ml corresponding
#+ to what the 30ml flask should have had   

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)


#+ carga el archivo y convierte los fragmentos de 16 /ul a fragmentos /ml 

qpcr<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/qPCR/qPCR.ods", sheet = "All" )%>%
  mutate_at(vars(`A-Fragmentos_16S_uL`, `B-Fragmentos_16S_uL`, `C-Fragmentos_16S_uL`), ~ . * 1000)%>%
  rename(`A_Fragmentos_16S_mL`=`A-Fragmentos_16S_uL`,
         `B_Fragmentos_16S_mL`=`B-Fragmentos_16S_uL`,
         `C_Fragmentos_16S_ml`=`C-Fragmentos_16S_uL`)



#+ correlation between the three replicates given by the three std curves
#+ are highly similar. thf, from now on we will use the mean measurement 
#+ calculated in q for further analysis

correlation <- qpcr %>%
  summarise(
    cor_A_B = cor(A_Fragmentos_16S_mL, B_Fragmentos_16S_mL, method = 'pearson'),
    cor_A_C = cor(A_Fragmentos_16S_mL, C_Fragmentos_16S_ml, method = 'pearson'),
    cor_B_C = cor(B_Fragmentos_16S_mL, C_Fragmentos_16S_ml, method = 'pearson'),
    mean_cor = mean(c(cor_A_B, cor_A_C, cor_B_C), na.rm = TRUE)
  )
correlation


#+ Primero se hace la mean de las tres std curves. 
#+ Luego, las filas que tienen notas son esta nota: Se repitió. VER qpcr plots.
#+ Esas muestras se repitieron porque se comportaban raro. Las repeticiones 
#+ concuerdan con las otras réplicas por lo que se eliminaron las primeras 
#+ mediciones y se dejaron las repeticiones


q <- qpcr %>%
  rowwise()%>%
  mutate(Fragmentos_16S_ml = mean(c(A_Fragmentos_16S_mL,B_Fragmentos_16S_mL,C_Fragmentos_16S_ml)))%>%
  select(!c(A_Fragmentos_16S_mL, B_Fragmentos_16S_mL, C_Fragmentos_16S_ml)) %>%
  filter(is.na(notas)) %>%
  select(!notas)%>%
  as.data.frame()





#+ JUNTAR QPCR DATA + METADATA + ENSAMBLES QPCR YA EN UNA SOLA TABLA & ajustar 
#+ volumenes tanto de los ensambles como de los batches 

#+ Ajustes de volumen:
#+ Ensambles: de 1ml que sobró del protocolo ensambles hice extracciones y resuspendí
#+ en 100uL (diln1:10) y usé 1ull para hacer qPCR. las extracciones de DNA se 
#+ sacaron del mililitro restante del PASO10 de protocolo ensamble de tubo con 
#+ 2ml, esos 2ml se diluyeron 1:10 en PASO10.1 y luego se volvieron a diluir 1:2
#+ en PASO10.2 y al último se diluyeron 1:100 300ul:30ml. 
#+ 
#+ PLT PARA ENSAMBLES los frag/ml se tienen que dividir entre 20000. 


#+ Batches: de los 30ml de cultivo se extrajeron 2ml (vol/15),luego se hizo extraccion
#+ de esos 2ml y se resuspendio en 60uL. De esos 60uL se uso 1uL para hacer qPCR
#+ (vol/60).
#+ 
#+ PLT PARA BATCHES: los frag/ml se tienen que multiplicar por 15*60 (*900)


#+ METADATA
m <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv", header = TRUE,  sep='\t' )


#+ Hace dos df. Uno para las muestras de los batches (b) y otra para los ensambles (e). 
#+ y modifcia ambos para que sean lo mas similares posibles

b <- q %>%
  left_join(m, by ='label_final')%>%
  filter(!is.na(community)) %>%
  select(!c(label, batch, techrep))%>%
  mutate(Fragmentos_16S_ml = Fragmentos_16S_ml * 900)


#+ Pero hay un problema, los ensambles no tienen temp ni repbio, entonces 
#+ en temp_rep se recopiln todas las combinaciones de comunidad+temp+repbio 
#+ que son diferentes y eso se agrega con left join a los datos de los ensambles 
#+ para que se repitan las mediciones de cada comunidad tres veces por las temps
#+ y dos veces (o 3) por las repbios  

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")

temp_rep <- b %>%
  distinct(community, temp, repbio)


e <- q %>%
  left_join(m, by ='label_final') %>%
  filter(is.na(community))%>%
  mutate(community = communities) %>%
  mutate(timepoint=0, hrs =0) %>%
  select(Muestra, label_final, DNA_conc, Vol_qPCR, CT, date, Fragmentos_16S_ml,
         community, timepoint, hrs) %>%
  left_join(
    temp_rep,
    by = "community")%>%
  mutate(Fragmentos_16S_ml = Fragmentos_16S_ml/20000)



#+ All. joins both df 
a <- b %>%
  full_join(e)


#+ Arrengea las comunidades en el df por orden 
a$community <- factor(a$community, levels = communities)



write.table(a, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = TRUE)



