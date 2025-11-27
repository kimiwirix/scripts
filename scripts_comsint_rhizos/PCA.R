
library(tidyverse)
library(readODS)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(vegan)
library(ape)
library(ggnewscale)


#cambiar dependiendo el archivo 
frequency_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/f_clean.tsv', sep = '\t', header = TRUE, row.names = "row.names")

proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 
proportion_table<-proportion_table %>%   
  t() %>%
  as.data.frame()

proportion_table$label<-rownames(proportion_table) #pone como primera columna los rownames para mergear con las otras tablas



#METADATA 
metadata_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/metadata_clean.tsv', sep = '\t', header = TRUE)

#merge 
proportion_metadata<-merge(proportion_table, metadata_table, by = "label") #mergea metadata con proportion table por el label name 

proportion_metadata[, 2:10] <- apply(proportion_metadata[,2:10], 2, as.numeric) #las columnas que tienen numeros de frequencias los convierte a numeric
proportion_metadata$group<-ifelse(proportion_metadata$exp %in% c("NS1", "NS3"), "Group1", "Group2") #agrupa NS1 y NS3 en Group1 y NS2 y NS4 en group 2 
proportion_metadata



t<-proportion_metadata

strains<- c("ST00101", "ST00154","NS_042g_27F","NS_164C_27F","ST00060","ST00046","ST00143","NS_110C_1_27F","ST00094","ST00109")
freqs<-t%>%
  select(all_of(strains))
meta<-t%>%
  select(!all_of(strains))%>%
  mutate(hrs = as.numeric(hrs))

pca <- prcomp(freqs, center = TRUE)
summary(pca)


p <- pca$x %>%
  bind_cols(meta)


#First component of all data is explained by thr two different groups (red and blue)
ggplot(data = p, aes(x=PC1, y=PC2))+
  geom_point(aes(color=as.factor(group)))
ggplot(data = p, aes(x=as.factor(group), y=PC1))+
  geom_boxplot()+
  geom_point(position = position_jitter(width = 0.2)) +
  labs( x = " ()", y="PC (%)") 

#Second component could be explained by relation time temperature thf we do a separete pca for the groups
ggplot(data = p, aes(x=as.factor(hrs), y=PC2))+
  facet_wrap(~temp)+
  geom_boxplot()+ #do another pca separating by group for cleaner results 
  geom_point(position = position_jitter(width = 0.2)) +
  labs( x = " ()", y="PC (%)") 




# thf make a separate pca for each of the two groups 
freqs_g1 <- freqs %>%
  bind_cols(meta) %>%
  filter(group=="Group1")%>%
  select(all_of(strains))
freqs_g2 <- freqs %>%
  bind_cols(meta) %>%
  filter(group=="Group2")%>%
  select(all_of(strains))


pca_g1 <- prcomp(freqs_g1, center = TRUE)
summary(pca_g1)
pca_g2 <- prcomp(freqs_g2, center = TRUE)
summary(pca_g2)


p_g1 <- pca_g1$x %>%
  bind_cols(meta%>%
              filter(group=="Group1"))

p_g2 <- pca_g2$x %>%
  bind_cols(meta%>%
              filter(group=="Group2"))

p_g1$temp<-as.factor(p_g1$temp)
p_g2$temp<-as.factor(p_g2$temp)


#Variation in group 1 (blue), can be explained by temperature
plot1<-ggplot(data = p_g1%>%
         filter(!is.na(temp)), aes(x=PC1, y=PC2))+
  geom_point(aes(color=temp, shape=temp), size=2)+
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(group = temp, color = temp, fill = temp), type = "norm", size = 0.5) +
  scale_fill_manual(values = c("28"="#63B8FF", "32"="lightsalmon"), name = "Temperature °C")+
  scale_color_manual(values = c("28"="#63B8FF", "32"="lightsalmon"), name = "Temperature °C")+
  scale_shape_manual(values = c("32" = 16, "28" = 17 ), name = "Temperature °C")+
  labs( x = "PC1 (58.7%)", y="PC2 (19.6%)", title="PCA - Group 1")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  

plot1
ggsave(plot1,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/PCA/group1_temp_pca.png" ,
       bg="white",  width = 20, height = 14, units = "cm")



ggplot(data = p_g1%>%
         filter(!hrs==0), aes(x=as.factor(temp), y=PC2))+
  facet_wrap(~hrs)+
  geom_boxplot()+
  geom_point(position = position_jitter(width = 0.2)) +
  labs( x = " temp", y="PC2 (%)") 



#Some variation in group 2 (red), can be explained by the colonization of few bacteria 
plot2<-ggplot(data = p_g2%>%
         filter(!is.na(temp)), aes(x=PC1, y=PC2))+
  geom_point(aes(color=community))+
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(group = community, color = community, fill = community), type = "norm", size = 0.5) +
  labs( x = "PC1 (44.9%)", y="PC2 (28.4%)", title="PCA - Group 2", color = "Community", fill = "Community")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 

plot2
ggsave(plot2,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/PCA/group2_community_pca.png" ,
       bg="white",  width = 20, height = 14, units = "cm")







