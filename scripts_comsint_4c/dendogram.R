#tengo matriz de identidad pyani: que tanto se parecen entre si los 78 genomas de 4c (dendogram)

library(readODS)
library(tibble)
library(ggplot2)


#id_mat<-read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/pyani_matrixes/matrix_identity_1.tab" ,sep = "\t", header = TRUE)
id_mat<-read.table("C:/Users/natal/Documents/LIIGH/pyani_report/matrix_identity_1.tab" ,sep = "\t", header = TRUE)


id_mat <- id_mat %>% 
  column_to_rownames(var = "X")  # Set 'RowNames' as the row names

distance_mat<-1-id_mat
distance_mat<-as.dist(distance_mat)
cluster_mat<-hclust(distance_mat)



png(file="C:/Users/natal/Documents/LIIGH/docs/docs_comsint_4c/dendogram.png",
    width=32,height=18,units="cm",res=1200)
plot(cluster_mat, cex=0.6)
dev.off()


