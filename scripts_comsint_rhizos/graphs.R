#grafica las frequencies de las cepas dentro de cada comsint(R1-R12) en los días (0-2) y compara las graficas entre temps (28 y 32)
#opción de gráfica de dispersión o por barplot de frecuencias 
#all changes of frequencies in every community 

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


#METADATA 
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

NS<-rbind(NS1,NS2,NS3,NS4)


#TABLE AND METADATA MERGED
metadata <- NS[NS$techrep == "A", ]
proportion_metadata<-merge(proportion_table, metadata, by = "label") #mergea metadata con proportion table por el label name 


communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
custom_colors <- c("ST00046" = "#25383C", "ST00154" = "#08519C", "ST00101" = "#00E5EE","ST00109" = "#4682B4", "ST00042" = "#BDD7E7",
                   "ST00143" = "#8B2323", "ST00164" = "#CD2626", "ST00094" = "#FF3030","ST00110" = "#EE6A50", "ST00060" = "#FCAE91")



options(scipen=999)#quita scientific notation 
all_tables_filtered<-data.frame()


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

#df_new duplica las filas que tienen NA de temp, y a una le pone 28 y a otra 32 para que aparezcan dentro del plot en tiempo 0
df_new <- all_tables_filtered %>%
  filter(temp=="NA") %>%
  uncount(2) %>%
  mutate(temp = rep(c("28", "32"), length.out = n()))

avg_per_comm_strain <- all_tables_filtered %>% #de la tabla por strain quita día 0-temp NA, y agrupa por comunidad y temp los values y saca el promedio de values por cada temp en cada community
  rbind(df_new) %>%
  filter(temp != "NA") %>%
  group_by(strain ,community , temp, hrs) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")


avg_per_comm_strain$community <- factor(avg_per_comm_strain$community, levels = communities) # Custom order

avg_NS_boxplot<-ggplot(data=avg_per_comm_strain, aes(fill = strain, y = mean_value, x = hrs))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = custom_colors)+
  facet_wrap(~community + temp,  nrow = 4, ncol = 6)+
  scale_x_continuous(breaks = c(0, 24, 48)) +
  labs(title = "Community composition through time and temperature",
       y = "Abundance", x = "Time (hrs)", fill="Strain") + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
  
avg_NS_boxplot

ggsave(avg_NS_boxplot,filename="avg_NS_barplot.png" , bg="white",  width = 30, height = 14, units = "cm")










########################BARPLOTS INDIVIDUALES, PERO SIN FACET WRAP, PURO FOR LOOP :0




NS1_3<-rbind(NS1,NS3)
NS2_4<-rbind(NS2,NS4)
NS<-c("NS1_3","NS2_4")#combina el NS1 con NS3 y NS2 con NS4

for (id in NS) {
  filtered_data_techrep <- get(id)[get(id)$techrep == "A", ] #para cada id (NS1y3,NS2y4) solo selecciona el techrep A    
  #temp_28 tiene las de 28 y día 0 (NA) y la 32 tiene las temps 32 y día 0 (NA)
  temp_28<-filtered_data_techrep[filtered_data_techrep$temp==28|filtered_data_techrep$temp=="NA",]
  temp_32<-filtered_data_techrep[filtered_data_techrep$temp==32|filtered_data_techrep$temp=="NA",]
  
  #Combina la frequency table con la de temperaturas por NS id
  combined_df_28 <- merge(proportion_table_df, temp_28, by = "label")
  combined_df_32 <- merge(proportion_table_df, temp_32, by = "label")
  
  #guarda las tablas en los nombres 
  name28<-paste(id,'28',sep = '_'); assign(name28, combined_df_28)
  name32<-paste(id,'32',sep = '_'); assign(name32, combined_df_32)
}


custom_colors <- c("ST00046" = "#25383C", "ST00154" = "#08519C", "ST00101" = "#00E5EE","ST00109" = "#4682B4", "ST00042" = "#BDD7E7",
                   "ST00143" = "#8B2323", "ST00164" = "#CD2626", "ST00094" = "#FF3030","ST00110" = "#EE6A50", "ST00060" = "#FCAE91")




options(scipen=999)#quita scientific notation 

#se pueden hacer fxn 
for (community in c("R1","R2","R3","R4","R5","R6")){ #loop de cada comunidad por cada temperatura 
  
  plot_list<-list()
  
  for (id in c("NS1_3_28","NS1_3_32")){
    
    table_per_community<-get(id)[get(id)$community==community,] #hace una tabla de cada comunidad R 
    table_per_community_ST<-table_per_community[,grepl("ST", names(table_per_community)) | names(table_per_community)=="hrs"] #solo deja columnas de frecuencias ST y columna de hrs 
    #en nuevo df _filtered pone columnas de df anterior que colsums sea mayor a 0.001 y que en las row de horas, en la hora 0 las colsums sea mayor que 0.001
    #a veces aparece numero en row pero son porcentajes muy muy bajos
    table_per_community_filtered<-table_per_community_ST[,which((colSums(table_per_community_ST)>0.001) & 
                                                          (colSums(table_per_community_ST[table_per_community_ST$hrs==0,]>0.001)))]
    
    #assamblea un nuevo df con el anterior filtrado y las columnas de info
    table_per_community_whole <- cbind(table_per_community_filtered, table_per_community[, c("community", "hrs", "temp","techrep","exp", "label")])
    #table_per_community<-table_per_community[, colSums(table_per_community != 0) > 0] #Quita las columnas que tengan puros ceros
    melt_data <- melt(table_per_community_whole, id = c('label', 'hrs', 'community','techrep','temp','exp'), variable.name = 'strain')#reorganiza tabla por strains 
    
    plot_title<-paste(substr(id, nchar(id)-1, nchar(id)),"°C",sep = "")  #xtrae los ultimos dos caracteres de NS1_3_28 y 32 para poner solo la temperatura 
    temp_plot<-ggplot(melt_data, aes(fill = strain, y = value, x = hrs))+ 
      geom_bar(position = "stack", stat = "identity")+
      scale_fill_manual(values = custom_colors)+
      ylim(0,1)+
      labs(x="Time (hrs)",
           y="Frequency",
           color="Strain")+
      scale_x_continuous(breaks = c(0, 24, 48)) +
      theme_minimal()+
      ggtitle(plot_title)+
      theme(plot.title = element_text(hjust = 0.5))#centra titulo
    
    temp_plot<-temp_plot+facet_grid(~exp)
    
    ##FOR DOTTED GRAPHS 
    # #usa tabla melt data, cada linea es una cepa diferente, cada tipo de linea (lisa o dotted) es un experimento diferente (NS1 o 3)
    # plot_title<-paste(substr(id, nchar(id)-1, nchar(id)),"°C",sep = "")  #xtrae los ultimos dos caracteres de NS1_3_28 y 32 para poner solo la temperatura 
    # temp_plot<-ggplot(melt_data, aes(x = hrs, y = value, color = strain, linetype = exp)) +
    #   geom_line(linewidth = 0.8) +
    #   scale_color_manual(values = custom_colors) +
    #   scale_linetype_manual(values = c("solid", "dotted")) +
    #   labs(
    #     title = plot_title,
    #     x = "Time (hrs)",
    #     y = "Frequency",
    #     color = "Strain",
    #     linetype = "Experiment"
    #   )+
    #   ylim(0,1)+
    #   scale_x_continuous(breaks = c(0, 24, 48)) +
    #   theme_minimal()+
    #   theme(plot.title = element_text(hjust = 0.5))#centra titulo
      
    plot_list[[id]] <- temp_plot #guarda el plot de la misma comunidad en las dos temps en una lista para luego hacer un plot con los dos plots 28 y 32
    
  }
  combined_plot <- plot_list[["NS1_3_28"]] + plot_list[["NS1_3_32"]] +
    plot_annotation(title = paste("Community", community),
    theme = theme(plot.title = element_text(hjust = 0.5)))#centra titulo
    
  
  name<-paste("barplot", community, sep = '_')
  assign(name,combined_plot)
  ggsave(combined_plot,filename=paste(name,".png", sep = "") , width = 30, height = 14, units = "cm")
}

for (community in c("R7","R8","R9","R10","R11","R12")){
  
  plot_list<-list()
  
  for (id in c("NS2_4_28","NS2_4_32")){
    
    table_per_community<-get(id)[get(id)$community==community,] #hace una tabla de cada comunidad R 
    table_per_community_ST<-table_per_community[,grepl("ST", names(table_per_community)) | names(table_per_community)=="hrs"]
    table_per_community_filtered<-table_per_community_ST[,which((colSums(table_per_community_ST)>0.001) & 
                                                                  (colSums(table_per_community_ST[table_per_community_ST$hrs==0,]>0.001)))]
    
    table_per_community_whole <- cbind(table_per_community_filtered, table_per_community[, c("community", "hrs", "temp","techrep","exp", "label")])
    melt_data <- melt(table_per_community_whole, id = c('label', 'hrs', 'community','techrep','temp','exp'), variable.name = 'strain')
    
    #proportion barplot
    plot_title<-paste(substr(id, nchar(id)-1, nchar(id)),"°C",sep = "")  #xtrae los ultimos dos caracteres de NS1_3_28 y 32 para poner solo la temperatura 
    temp_plot<-ggplot(melt_data, aes(fill = strain, y = value, x = hrs))+ 
      geom_bar(position = "stack", stat = "identity")+
      scale_fill_manual(values = custom_colors)+
      ylim(0,1)+
      labs(x="Time (hrs)",
           y="Frequency",
           color="Strain")+
      scale_x_continuous(breaks = c(0, 24, 48)) +
      theme_minimal()+
      ggtitle(plot_title)+
      theme(plot.title = element_text(hjust = 0.5))#centra titulo
    
    temp_plot<-temp_plot+facet_grid(~exp)
    
    plot_list[[id]] <- temp_plot
    
  }
  combined_plot <- plot_list[["NS2_4_28"]] + plot_list[["NS2_4_32"]] +
    plot_annotation(title = paste("Community", community),
                    theme = theme(plot.title = element_text(hjust = 0.5)))
  
  name<-paste("barplot", community, sep = '_')
  assign(name,combined_plot)
  ggsave(combined_plot,filename=paste(name,".png", sep = "") , width = 30, height = 14, units = "cm")
  
}

table_per_community




