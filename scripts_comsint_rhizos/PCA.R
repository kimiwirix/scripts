library(readODS)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(vegan)
library(ape)

frequency_table<-read_ods("C:/Users/natal/Documents/LIIGH/16S analysis/total_abundance_table.ods")
frequency_table<-as.matrix(frequency_table)
colnames(frequency_table) <- ifelse(grepl("^N5", colnames(frequency_table)), sub("5", "S", colnames(frequency_table)), colnames(frequency_table)) #cambia el primer 5 a S de solo los que empiezan con N5
rownames(frequency_table)<-frequency_table[,1] #pone ids como rownames y los quita de la columna 1
frequency_table<- frequency_table[,-1]#quita primera columna de ids
class(frequency_table)<-"numeric"

proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 
proportion_table
options(scipen=999)#quita scientific notation 

proportion_table<-t(proportion_table)#transpose

proportion_table
NS1<-read_ods("C:/Users/natal/Documents/LIIGH/collection_sheet.ods", sheet="full")
NS1<-as.data.frame(NS1)
NS1<-NS1[c("community","hrs","temp","label","techrep","exp")]#solo deja el dataframe con esas columnas 

NS2<-read_ods("C:/Users/natal/Documents/LIIGH/collection_sheet_NS2.ods", sheet="full")
NS2<-as.data.frame(NS2)
NS2<-NS2[c("community","hrs","temp","label","techrep","exp")]

NS3<-read_ods("C:/Users/natal/Documents/LIIGH/collection_sheet_NS3.ods", sheet="full")
NS3<-as.data.frame(NS3)
NS3<-NS3[c("community","hrs","temp","label","techrep","exp")]

NS4<-read_ods("C:/Users/natal/Documents/LIIGH/collection_sheet_NS4.ods", sheet="full")
NS4<-as.data.frame(NS4)
NS4<-NS4[c("community","hrs","temp","label","techrep","exp")]


NS<-rbind(NS1,NS3,NS2,NS4)


metadata <- NS[NS$techrep == "A", ]
proportion_table<-cbind(label=rownames(proportion_table), proportion_table) #pone rownames como primera columna y los llama label 
proportion_metadata<-merge(proportion_table, metadata, by = "label") #mergea metadata con proportion table por el label name 

proportion_metadata[, 2:10] <- apply(proportion_metadata[,2:10], 2, as.numeric) #las columnas que tienen numeros de frequencias los convierte a numeric
proportion_metadata$group<-ifelse(proportion_metadata$exp %in% c("NS1", "NS3"), "Group1", "Group2") #agrupa NS1 y NS3 en Group1 y NS2 y NS4 en group 2 
proportion_metadata


#PCA
pca <- prcomp(proportion_metadata[,c(2:10)])#solo hace pca de columnas con frequencies
autoplot(pca, data = proportion_metadata, colour="group", shape="group")


#PCA of groups: 1(NS1,NS3) and 2(NS2,NS4)
group1<-proportion_metadata[proportion_metadata$group=="Group1",]
pca_group1 <- prcomp(group1[ ,c(2:10)])#solo hace pca de columnas con frequencies
autoplot(pca_group1, data = group1, colour="temp")

group2<-proportion_metadata[proportion_metadata$group=="Group2",]
pca_group2 <- prcomp(group2[ ,c(2:10)])#solo hace pca de columnas con frequencies
autoplot(pca_group2, data = group2, colour="temp")
new<-proportion_metadata[,c(2:10,17:37)]



###############sur
pca


library(tidyverse)


groups <- c("Group1", "Group2")
groups <- c("Group1")
# groups <- c("Group2")

Freq <- proportion_metadata[proportion_metadata$group %in% groups, ][,2:10]
Meta <- proportion_metadata[proportion_metadata$group %in% groups, ][, -(2:10)]
Meta <- Meta %>%
  mutate(hrs = as.numeric(hrs))

Freq.pca <- prcomp(Freq)
summary(Freq.pca)


Dat <- Freq.pca$x %>% as_tibble() %>%
  bind_cols(Meta)


library(ggnewscale)
Dat %>%
  ggplot(aes(x = PC1, y = PC2)) +
  labs(title = "PCA", y = "PC2 (28.95%)", x = "PC1 (36.86%)")+
  

  
  geom_point(data = Dat %>% filter(temp == "28"),
             aes(fill = hrs), shape = 21, size = 4) +
  scale_fill_gradient(low = "#74add1", high = "#e0f3f8", name = "28°C") + 
 
  
  new_scale_fill() +
  
  
  geom_point(data = Dat %>% filter(temp == "32"),
             aes(fill = hrs), shape = 21, size = 4) +
  scale_fill_gradient(low = "#f46d43", high = "#ffffbf", name = "32°C") +
  
  geom_point(data = Dat %>% filter(temp == "NA"),
             fill = "black", shape = 21, size = 4) +
    
  theme_classic()

  

Dat %>%
  mutate(hrs = factor(hrs)) %>%
  mutate(temp = factor(temp, levels = c("NA", "28", "32"))) %>%
  ggplot(aes(x = hrs, y = PC2)) +
  facet_grid(~temp, scales = "free_x", space = "free_x") +
  geom_boxplot(outlier.color = NA) + 
  geom_point(position = position_jitter(width = 0.2)) +
  labs( x = "Time (hrs)", y="PC2 (28.95%)") +
  theme_classic()


communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")

##comm
Dat %>%
  filter(temp!="NA")%>%
  mutate(hrs = factor(hrs)) %>%
  mutate(temp = factor(temp, levels = c( "28", "32"))) %>%
  mutate(community = factor(community, levels = communities)) %>%
  ggplot(aes(x = temp, y = PC1)) +
  facet_grid(~community, scales = "free_x", space = "free_x") +
  geom_boxplot(outlier.color = NA) + 
  geom_point(position = position_jitter(width = 0.2)) +
  labs( x = "Time (hrs)") +
  theme_classic()

Dat






varpart_result <- varpart(Freq, ~ community  , ~ hrs+temp , data = Meta)
plot(varpart_result)
summary(varpart_result)









que hacer para ppt: 
  *hacer grafico de baras en boxplot con cada punt que sea una comsint NS 
  *displayear en ppt PC1 y PC2 con x axis como tiempo, PC1 muestra cambio de media confomre al tiempo 
  PC2 muestra cambio en varianza conforme al tiempo pero tanbien un ligero aumento en media en 28°C 




  
  
  
  
  

  
unifrac
phyloseq



FALTA 

yo
hacer comsints y congelar solo la pstilla sin rna later 

checar
curvas de crecimiento: crecer alicuota de overnight culture en volumen y tomar cada 30min OD 
  
ppt
intro
diagrama 
pca
barplots promedios entre ns
efecto temp y efecto cepas

checar para ppt: 
***ancom-bc que comsints cambian dependiendo de que factiroes? 
**** capscale
***cambios cepas por comsint 
