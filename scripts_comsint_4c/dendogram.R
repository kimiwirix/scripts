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



<<<<<<< Updated upstream
png(file="C:/Users/natal/Documents/LIIGH/docs/docs_comsint_4c/dendogram.png",
    width=32,height=18,units="cm",res=1200)
plot(cluster_mat, cex=0.6)
=======
colors<-c("red", "blue", "green","orange", "purple", "yellow", "cyan", "darkgreen", "magenta", "darkblue",  "pink", "darkred", "gray", "gold", "brown")



cut_tree<-cutree(cluster_mat,k=10, order_clusters_as_data = FALSE) #parses tree in k parts
table(cut_tree) #summary of how many organims in each cut part
cut_tree[cut_tree==1] #which organisms are in part 1


png(file="C:/Users/natal/Documents/LIIGH/docs/docs_comsint_4c/parsed_dendogram.png",
    width=32,height=18,units="cm",res=1200)

par(mar = c(9,2,1,0.5) + 0.6) #makes window bigger so labels can fit 

cluster_mat%>%
  as.dendrogram()%>%
  color_branches(.,
                 k=10,
                 col=colors,
                 groupLabels = TRUE)%>%
  set("labels_cex", 0.6)%>%
  hang.dendrogram(hang = -1)%>%
  plot


>>>>>>> Stashed changes
dev.off()


