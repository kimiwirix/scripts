#same bacteria in differente communities 

library(readODS)
library(reshape2)
library(ggplot2)
library(patchwork)
library(remotes)
library(sjstats)
library(dplyr)
library(tibble)


#cambiar dependiendo el archivo 
frequency_table<-read_ods("C:/Users/natal/Documents/LIIGH/16S analysis/total_abundance_table.ods")
frequency_table<-frequency_table %>%
  column_to_rownames(var="RowName") %>%
  as.matrix()
colnames(frequency_table) <- ifelse(grepl("^N5", colnames(frequency_table)), sub("5", "S", colnames(frequency_table)), colnames(frequency_table)) #cambia el primer 5 a S de solo los que empiezan con N5

proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 
proportion_table<-proportion_table %>%   
  t() %>% 
  as.data.frame()

proportion_table$label<-rownames(proportion_table) #pone como primera columna los rownames para mergear con las otras tablas


#metadata 
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
#NS2_4<-rbind(NS2,NS4)

metadata <- NS[NS$techrep == "A", ]
proportion_metadata<-merge(proportion_table, metadata, by = "label") #mergea metadata con proportion table por el label name 
proportion_metadata

strains<-c("ST00042","ST00046","ST00060","ST00094","ST00101","ST00109", "ST00143","ST00154","ST00164")
communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
custom_colors <- c("ST00046" = "#25383C", "ST00154" = "#08519C", "ST00101" = "#00E5EE","ST00109" = "#4682B4", "ST00042" = "#BDD7E7",
                   "ST00143" = "#8B2323", "ST00164" = "#CD2626", "ST00094" = "#FF3030","ST00110" = "#EE6A50", "ST00060" = "#FCAE91")




options(scipen=999)#quita scientific notation 
all_tables_filtered<-data.frame()


#for que filtra las cepas que van en cada comsint y las agupa en una df general (all_tables_filtered)
for (comsint in communities){ #loop de cada comunidad por cada temperatura 
  table_per_community<-proportion_metadata[proportion_metadata$community==comsint,] #hace una tabla de cada comunidad R 
  table_per_community_ST<-table_per_community[,grepl("ST", names(table_per_community)) | names(table_per_community)=="hrs"] #solo deja columnas de frecuencias ST y columna de hrs 
  #en nuevo df _filtered pone columnas de df anterior que colsums sea mayor a 0.001 y que en las row de horas, en la hora 0 las colsums sea mayor que 0.001
  #a veces aparece numero en row pero son porcentajes muy muy bajos
  table_per_community_filtered<-table_per_community_ST[,which((colSums(table_per_community_ST)>0.001) & 
                                                                (colSums(table_per_community_ST[table_per_community_ST$hrs==0,]>0.001)))]
  table_per_community_whole <- cbind(table_per_community_filtered, table_per_community[, c("community", "hrs", "temp","techrep","exp", "label")])
  melt_data <- melt(table_per_community_whole, id = c('label', 'hrs', 'community','techrep','temp','exp'), variable.name = 'strain')#reorganiza tabla por strains 
  
  all_tables_filtered<-rbind(all_tables_filtered, melt_data) #df con info filtrada: cada cepa aparece en la comsint que debe aparecer 
}


#######individual barplots

avg_per_comm_strain <- all_tables_filtered %>%   #de la tabla por strain quita temps de día 0, y agrupa por comunidad y temp los values y saca el promedio de values por cada temp en cada community
  filter(temp != "NA") %>% 
  group_by(strain ,community , temp) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")


#https://www.zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/
strains_blue<-avg_per_comm_strain %>%   #de la tabla por strain quita temps de día 0, y agrupa por comunidad y temp los values y saca el promedio de values por cada temp en cada community
  filter(strain %in% c("ST00046", "ST00154", "ST00101","ST00109" , "ST00042"))  

strains_red<-avg_per_comm_strain %>%   #de la tabla por strain quita temps de día 0, y agrupa por comunidad y temp los values y saca el promedio de values por cada temp en cada community
  filter(strain %in% c("ST00143", "ST00164", "ST00094","ST00110" , "ST00060"))  



plot_blue<-ggplot(data = strains_blue, aes(x = factor(community, level=communities), y=mean_value)) +
  geom_bar(stat = "identity", aes(fill = strain))+#, position = "fill") + 
  labs(title = "Strains in communities",
       y = "Total Abundance", x = "Community") + 
  theme_minimal()+
  facet_wrap(~ strain + temp, axes = "all_x")+
  scale_fill_manual(values=custom_colors)+
  #ylim(0,5)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
        axis.title.x = element_text(size = 12),  # Customize x-axis title
        strip.text = element_text(size = 10),  # Customize facet labels
        panel.spacing = unit(1, "lines")  # Add space between facets to avoid overlaps
        )
  

plot_red<-ggplot(data = strains_red, aes(x = factor(community, level=communities), y=mean_value)) +
  geom_bar(stat = "identity", aes(fill = strain))+#, position = "fill") + 
  labs(title = "Strains in communities",
       y = "Total Abundance", x = "Community") + 
  theme_minimal()+
  facet_wrap(~ strain + temp, axes = "all_x")+
  scale_fill_manual(values = custom_colors)+
  #ylim(0,5)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels for better readability
        axis.title.x = element_text(size = 12),  # Customize x-axis title
        strip.text = element_text(size = 10),  # Customize facet labels
        panel.spacing = unit(1, "lines")  # Add space between facets to avoid overlaps
  )



ggsave(filename = "plot_blue.png", plot = plot_blue, bg="white", width = 30, height = 14, units = "cm")
ggsave(filename = "plot_red.png", plot = plot_red, bg="white", width = 30, height = 14, units = "cm")




##########boxplot  

all_tables_filtered
filtered_data <- all_tables_filtered[all_tables_filtered$temp %in% c(28, 32), ] #not including Nas


boxplot_all_strains<-ggplot(data= filtered_data, aes(x=temp,y=value))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~strain)+
  labs(title = "Strains in communities",
       y = "Abundance", x = "Temperature") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


boxplot_all_strains
ggsave(filename = "boxplot.png", plot = boxplot_all_strains, bg="white", width = 30, height = 14, units = "cm")





####change in temperature does not have a statistically significant effect on the value for any of the communities

community_tests <- filtered_data %>%
  group_by(community)%>%
  summarize(
    p_value = wilcox.test(value[temp == 28], value[temp == 32], exact = FALSE)$p.value
  )

community_tests <- community_tests %>%
  mutate(significant = ifelse(p_value < 0.05, "Yes", "No"))

community_tests


