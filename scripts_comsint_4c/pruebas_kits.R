library(readODS)
library(reshape2)
library(ggplot2)
library(patchwork)
library(remotes)
library(sjstats)
library(dplyr)
library(tibble)
library(tidyr)
library(ggpubr)


#cambiar dependiendo el archivo 
freq_table_matched<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/pruebas_kits/with_448b/feature-table-open-all.ods", sheet = "matched")%>%
  janitor::row_to_names(row_number = 1)%>%
  column_to_rownames(var="#OTU ID")%>%
  as.data.frame()%>%
  mutate(across(everything(), ~ as.numeric(unlist(.))))
freq_table_unmatched<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/pruebas_kits/with_448b/feature-table-open-all.ods", sheet = "unmatched") %>%
  janitor::row_to_names(row_number = 1)%>%
  column_to_rownames(var="#OTU ID")%>%
  as.data.frame()%>%
  mutate(across(everything(), ~ as.numeric(unlist(.))))



#creates frequency table with frequencies and proportions of each strain in the community using the different kits
u<-colSums(freq_table_unmatched)%>%
  as.data.frame()%>%
  t()
rownames(u)<-"unmatched"

  
f<-freq_table_matched%>%
  bind_rows(as.data.frame(u))%>%
  rownames_to_column(var = "strain")%>%
  melt(variable.name = 'kit', value.name = "freq")%>%
  group_by(kit)%>%
  mutate(prop=freq/sum(freq))


#change between freq and prop in y axis

plot_f<-ggplot(data = f, aes(x=kit, y = freq, fill = strain))+ geom_bar(position = "stack", stat = "identity")+ scale_fill_brewer(palette="Paired")
plot_p<-ggplot(data = f, aes(x=kit, y = prop, fill = strain))+ geom_bar(position = "stack", stat = "identity")+ scale_fill_brewer(palette="Paired")

plots<-ggarrange(plot_f, plot_p ,
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "right")


ggsave(plots,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/pruebas_kits.png" ,
       bg="white",  width = 30, height = 14, units = "cm")


head(f)


#none unmatched samples are above the 10% of the mean of the matched samples
freq_table_unmatched
overall_mean<-mean(as.matrix(freq_table_matched))

freq_table_unmatched%>%
  filter(if_any(everything(), ~ . >= (overall_mean*0.10)))








