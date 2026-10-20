
#+ -----------------------------------------------------------------------------
#+ Master database of qPCRs from experiments, incorporates metadata, and ensamble 
#+ information in the final table. 
#+ 
#+ Makes a regression of the three std curves per assay to calculate the 
#+ Fragmentos16S based on the CT of the samples
#+ 
#+ And adjusts volumes to Frag/ml corresponding
#+ to what the 30ml flask should have had   
#+ 

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)




#+ En el excel qpcr_16S se agregó una hoja con la info de las tres std curves 
#+ por assay, con esa info se va a hacer una regression line de las tres std 
#+ curves por assay 
#+ 
#+ en s se agregan las columnas de fragmentos de las std curves basados en la 
#+ concentración (el número 10400000 carina lo sacó) 
#+ 
#+ en regress_s salen tres nuevas columnas con datos de pendiente(m) e 
#+ intercepto(b) de un modelo lineal que correlaciona las 3 replicas ABC de cada 
#+ assay

std <- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/qPCR/qPCR_16S.ods", sheet = "STD_CURVES_INFO" )

s <- std %>%
  mutate(STD_Fragmentos16S_uL = Std_quantity * 1046000000/1, 
         log_conc_DNA = log10(STD_Fragmentos16S_uL)) 

regress_s <- s %>%
  group_by(qPCR_assay) %>%
  mutate(
    m = coef(lm(Std_CT ~ log_conc_DNA))[2],
    b = coef(lm(Std_CT ~ log_conc_DNA))[1], 
    r2 = summary(lm(Std_CT ~ log_conc_DNA))$r.squared
  ) %>%
  distinct(qPCR_assay, m, b, r2)





#+ carga el archivo y une los datos de la regrsión de las std curves para 
#+ calcular los frag_ul con los CTs de las muestras 

qpcr<- read_ods(path ="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/qPCR/qPCR_16S.ods", sheet = "All" )

q <- qpcr %>%
  left_join(regress_s, by = 'qPCR_assay') %>%
  mutate(
    Fragmentos16S_uL= 10^((CT-b)/m)) %>%
  filter(is.na(notas)) %>%
  select(!notas)%>%
  as.data.frame()



#+ Las filas que tienen notas son esta nota: Se repitió. VER qpcr plots.
#+ Esas muestras se repitieron porque se comportaban raro. Las repeticiones 
#+ concuerdan con las otras réplicas por lo que se eliminaron las primeras 
#+ mediciones y se dejaron las repeticiones



#+ CALCULTAES CORRELATION TO COMPROBAR QUE LAS TRES STD CURVES SE PARECEN
#+ 
#+ correlation between the three replicates given by the three std curves
#+ are highly similar. thf, from now on we will use the mean measurement 
#+ calculated in q for further analysis

correlation <- qpcr %>%
  summarise(
    cor_A_B = cor(`A-Fragmentos_16S_uL`, `B-Fragmentos_16S_uL`, method = 'pearson'),
    cor_A_C = cor(`A-Fragmentos_16S_uL`, `C-Fragmentos_16S_uL`, method = 'pearson'),
    cor_B_C = cor(`B-Fragmentos_16S_uL`, `C-Fragmentos_16S_uL`, method = 'pearson'),
    mean_cor = mean(c(cor_A_B, cor_A_C, cor_B_C), na.rm = TRUE)
  )
correlation







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
#+ 
#+ REVISION CON SUR: ENSMABLES SE DIVIDEN ENTRE 2


#+ Batches: de los 30ml de cultivo se extrajeron 2ml (vol/15),luego se hizo extraccion
#+ de esos 2ml y se resuspendio en 60uL. De esos 60uL se uso 1uL para hacer qPCR
#+ (vol/60).
#+ 
#+ PLT PARA BATCHES: los frag/ml se tienen que multiplicar por 15*60 (*900)
#+ 
#+ #+ REVISION CON SUR: BATCHES SE MULTIPLICAN POR 30


#+ METADATA
m <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv", header = TRUE,  sep='\t' ) %>%
  select(!date)


#+ Hace dos df. Uno para las muestras de los batches (b) y otra para los ensambles (e). 
#+ y modifcia ambos para que sean lo mas similares posibles

b <- q %>%
  left_join(m, by ='label_final')%>%
  filter(!is.na(community))%>%
  select(!c(label, techrep))%>%
  mutate(Fragmentos16S_uL=Fragmentos16S_uL * 30)


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
  filter(is.na(community)) %>%
  mutate(community = communities) %>%
  mutate(timepoint=0, hrs =0)%>%
  select(Muestra, label_final, DNA_conc, Vol_qPCR, CT, date, Fragmentos16S_uL , community, timepoint, hrs) %>%
  left_join(
    temp_rep,
    by = "community") %>%
  mutate(Fragmentos16S_uL=Fragmentos16S_uL/2)



#+ All. joins both dfs.
#+ Also takes the mean of the three curves just in case it is necessary for 
#+ further analysis 

a <- b %>%
  full_join(e) %>%
  rename(`Fragmentos16S_mL_regression`=`Fragmentos16S_uL`)



#+ Arrangea las comunidades en el df por orden 
a$community <- factor(a$community, levels = communities)


write.table(a, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = TRUE)



