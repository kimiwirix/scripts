
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
frequency_table<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches_99ident/feature-table-open-all-99ident.ods", sheet = "matched")
frequency_table<-frequency_table %>%
  column_to_rownames(var="#OTU ID") %>%
  as.matrix()
colnames(frequency_table) <- ifelse(grepl("^N5", colnames(frequency_table)), sub("5", "S", colnames(frequency_table)), colnames(frequency_table)) #cambia el primer 5 a S de solo los que empiezan con N5


proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 
proportion_table<-proportion_table %>%   
  t() %>%
  as.data.frame()

proportion_table$label<-rownames(proportion_table) #pone como primera columna los rownames para mergear con las otras tablas



#METADATA 
NS1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")

NS2<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS2.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")

NS3<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS3.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")

NS4<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS4.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "label", "techrep", "exp", "temp", "hrs")

NS<-rbind(NS1,NS2,NS3,NS4)


#NS that we had to use techrep B since A failed
labels_B<- c("NS00037B", "NS00038B", "NS00039B", "NS00040B", "NS00041B", "NS00042B",
             "NS00043B", "NS00044B", "NS00045B", "NS00046B", "NS00047B", "NS00048B",
             "NS00079B", "NS00080B", "NS00081B", "NS00082B", "NS00083B", "NS00084B",
             "NS00085B", "NS00086B", "NS00088B", "NS00089B", "NS00090B", "NS00165B",
             "NS00169B", "NS00171B", "NS00173B")
labels_A <- gsub("B$", "A", labels_B)


removedlabels<-c("NS00071A", "NS00072A", "NS00078A", "NS00079B", "NS00083B", "NS00084B", "NS00089B", "NS00090B", "NS00100A", "NS00118A", "NS00130A", "NS00161A", "NS00173B")


#quita las muetsras que son contaminacion en contamination_matched.r y filtra por día

metadata<-NS%>%
  filter(techrep=="A" | (techrep=="B" & label %in% labels_B)) %>%
  filter(!label %in% labels_A)%>%
  filter(!label == "NS00087A") %>% #lost sample:(
  filter(!(label %in% removedlabels))#quita las muetsras que son contaminacion

#merge 
proportion_metadata<-merge(proportion_table, metadata, by = "label") #mergea metadata con proportion table por el label name 

proportion_metadata[, 2:10] <- apply(proportion_metadata[,2:10], 2, as.numeric) #las columnas que tienen numeros de frequencias los convierte a numeric
proportion_metadata$group<-ifelse(proportion_metadata$exp %in% c("NS1", "NS3"), "Group1", "Group2") #agrupa NS1 y NS3 en Group1 y NS2 y NS4 en group 2 
proportion_metadata


#pca

groups <- c("Group1", "Group2")
#groups <- c("Group1")
#groups <- c("Group2")
#groups <- c("NA")#, "32")


Freq <- proportion_metadata[proportion_metadata$group %in% groups, ][,2:10]
Meta <- proportion_metadata[proportion_metadata$group %in% groups, ][, -(2:10)]
Meta <- Meta %>%
  mutate(hrs = as.numeric(hrs))

Freq.pca <- prcomp(Freq)
summary(Freq.pca)


Dat <- Freq.pca$x %>% as_tibble() %>%
  bind_cols(Meta)


#PCA GRAPH 
Dat %>%
  ggplot(aes(x = PC1, y = PC2)) +
  labs(title = "PCA", y = "PC2 (32.06%)", x = "PC1 (39.08%)")+
  
  
  
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






#PCA POR EXP 
#si quito comsint R10 y R5 se ve mas claro el plot 
Dat %>%
  ggplot(aes(x = PC1, y = PC2)) +
  labs(title = "PCA", y = "PC2 (%)", x = "PC1 (%)", fill="Category")+
  
  geom_point(data = Dat %>% filter(group == "Group1" & community!="R5"),
             shape = 20, size = 4, aes(colour  = "Exp1&3"))+
  geom_point(data = Dat %>% filter(group == "Group2" & community!="R10"),
             shape = 20, size = 4, aes(colour = "Exp2&4")) +
  geom_point(data = Dat %>% filter(community=="R1" | community=="R7"),
             shape = 20, size = 4, aes(colour = "R1&R7"))+
  geom_point(data = Dat %>% filter(hrs == 0 & group == "Group1"),
             shape = 20, size = 4, aes(colour = "Inoculum"))+
  geom_point(data = Dat %>% filter(hrs == 0 & group == "Group2"),
             shape = 20, size = 4, aes(colour = "Inoculum2"))+
  scale_color_manual(values = c("Exp1&3" = "red",
                                "Exp2&4" = "blue",
                                "R1&R7" = "black",
                                "Inoculum"="lightsalmon",
                                "Inoculum2"="lightblue")) +
  theme_classic()


 



#BOXPLOTS
Dat %>%
  mutate(hrs = factor(hrs)) %>%
  mutate(temp = factor(temp, levels = c("NA", "28", "32"))) %>%
  ggplot(aes(x = hrs, y = PC2)) + #change between PC1 and PC2
  facet_grid(~temp, scales = "free_x", space = "free_x") +
  geom_boxplot(outlier.color = NA) + 
  geom_point(position = position_jitter(width = 0.2)) +
  labs( x = "Time (hrs)", y="PC2 (30.75%)") +
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


