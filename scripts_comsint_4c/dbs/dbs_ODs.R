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

