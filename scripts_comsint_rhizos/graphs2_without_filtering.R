#same bacteria in different communities 

library(readODS)
library(reshape2)
library(ggplot2)
library(patchwork)
library(remotes)
library(sjstats)
library(dplyr)
library(tibble)
library(tidyr)


#cambiar dependiendo el archivo 
frequency_table<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches/feature-table-open-all-allbatches.ods", sheet = "matched")
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
NS1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet.ods", sheet="full")
NS1<-as.data.frame(NS1)
NS1<-NS1[c("community","hrs","temp","label","techrep","exp")]#solo deja el dataframe con esas columnas 

NS2<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS2.ods", sheet="full")
NS2<-as.data.frame(NS2)
NS2<-NS2[c("community","hrs","temp","label","techrep","exp")]

NS3<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS3.ods", sheet="full")
NS3<-as.data.frame(NS3)
NS3<-NS3[c("community","hrs","temp","label","techrep","exp")]

NS4<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS4.ods", sheet="full")
NS4<-as.data.frame(NS4)
NS4<-NS4[c("community","hrs","temp","label","techrep","exp")]

NS<-rbind(NS1,NS2,NS3,NS4)



#TABLE AND METADATA MERGED

#NS that we had to use techrep B since A failed
labels_B<- c("NS00037B", "NS00038B", "NS00039B", "NS00040B", "NS00041B", "NS00042B",
             "NS00043B", "NS00044B", "NS00045B", "NS00046B", "NS00047B", "NS00048B",
             "NS00079B", "NS00080B", "NS00081B", "NS00082B", "NS00083B", "NS00084B",
             "NS00085B", "NS00086B", "NS00088B", "NS00089B", "NS00090B", "NS00165B",
             "NS00169B", "NS00171B", "NS00173B")
labels_A <- gsub("B$", "A", labels_B)


metadata<-NS%>%
  filter(techrep=="A" | (techrep=="B" & label %in% labels_B)) %>%
  filter(!label %in% labels_A)%>%
  filter(!label == "NS00087A") #lost sample:(


proportion_metadata<-merge(proportion_table, metadata, by = "label") #mergea metadata con proportion table por el label name 

strains<-c("NS_042g_27F","ST00046","ST00060","ST00094","ST00101","ST00109", "ST00143","ST00154","NS_164C_27F","NS_110C_1_27F")
communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
custom_colors <- c("ST00046" = "#25383C", "ST00154" = "#08519C", "ST00101" = "#00E5EE","ST00109" = "#4682B4", "NS_042g_27F" = "#BDD7E7",
                   "ST00143" = "#8B2323", "NS_164C_27F" = "#CD2626", "ST00094" = "#FF3030","NS_110C_1_27F" = "#EE6A50", "ST00060" = "#FCAE91")



proportion_metadata

#Removed NAs (day 0) measurements from dataset 
proportion_metadata<-proportion_metadata%>%
  filter(temp!="NA")

melt_data <- melt(proportion_metadata, id = c('label', 'hrs', 'community','techrep','temp','exp'), variable.name = 'strain')#reorganiza tabla por strains 

melt_data <- melt_data %>% #de la tabla por strain quita día 0-temp NA, y agrupa por comunidad y temp los values y saca el promedio de values por cada temp en cada community
  group_by(strain ,community , temp, hrs)%>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")%>%
  as.data.frame()

melt_data$community <- factor(melt_data$community, levels = communities) # Custom order



boxplot<-ggplot(data= melt_data, aes(x=temp,y=mean_value))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~strain)+
  labs(title = "Strains in communities",
       y = "Abundance", x = "Temperature") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


  ggsave(filename = "C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/strains across comsints/boxplot_allbatches.png", 
         plot = boxplot, 
         bg="white", width = 30, height = 14, units = "cm")




###change in temperature does not have a statistically significant effect on the value for any of the communities

community_tests <- melt_data %>%
  group_by(community)%>%
  summarize(
    p_value = wilcox.test(mean_value[temp == 28], mean_value[temp == 32], exact = FALSE)$p.value)

community_tests <- community_tests %>%
  mutate(significant = ifelse(p_value < 0.05, "Yes", "No"))

community_tests

















