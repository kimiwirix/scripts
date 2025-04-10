#ANCOM-BC2

install.packages("devtools")
devtools::install_github("FrederickHuangLin/ANCOMBC") 
library(ANCOMBC)
library(dplyr)
library(tidyverse)
library(readODS)
library(reshape2)
 
options(scipen=999)
#matrix where rows are strains and columns are samples.
frequency_table<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/analisis/analisis_freq_allbatches_99ident/feature-table-open-all-99ident.ods", sheet = "matched")
frequency_table<-frequency_table %>%
  tibble::column_to_rownames(var="#OTU ID") %>%
  as.matrix()

colnames(frequency_table) <- ifelse(grepl("^N5", colnames(frequency_table)), sub("5", "S", colnames(frequency_table)), colnames(frequency_table)) #cambia el primer 5 a S de solo los que empiezan con N5

#proportion_table<-apply(frequency_table, 2, function(x) x/sum(x)) 


#metadata
NS1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community","hrs" ,"label", "techrep", "exp", "temp")
NS2<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS2.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community","hrs" , "label", "techrep", "exp", "temp")
NS3<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS3.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community", "hrs" ,"label", "techrep", "exp", "temp")
NS4<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/collection_sheet_NS4.ods", sheet="full")%>%
  as.data.frame()%>%
  select("community","hrs" , "label", "techrep", "exp","temp")


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


metadata<-metadata%>%
  column_to_rownames("label")%>%
  #mutate_at("temp", as.numeric)%>%
  mutate(color_comsint=case_when(exp=="NS1" | exp=="NS3" ~ "blue",
                                 exp=="NS2" | exp=="NS4" ~ "red"))%>%
  mutate(community_temp=case_when(temp=="NA" ~ paste0(community,"I"),
                                  temp=="28" ~ paste0(community, "L"),
                                  temp=="32" ~ paste0(community, "H")))
#  mutate_at("temp", as.numeric)





#REMOVED LABELS BASE ON HOW CONTAMINATED THEY WERE (contamination_matched.R)
removedlabels<-c("NS00071A", "NS00072A", "NS00078A", "NS00079B", "NS00083B", "NS00084B", "NS00089B", "NS00090B", "NS00100A", "NS00118A", "NS00130A", "NS00161A", "NS00173B")


#quita las muetsras que son contaminacion en contamination_matched.r y filtra por día
meta<-metadata%>%
  filter(!(row.names(metadata) %in% removedlabels))
  #filter(hrs=="24" | hrs=="0")


freq<-frequency_table%>%
  as.data.frame()%>%
  select(-all_of(removedlabels))%>%
  select(any_of(rownames(meta)))%>%
  as.matrix()

m1<-ancombc2(data = freq,
             taxa_are_rows = TRUE,
             meta_data = meta,
             fix_formula = "community_temp",
             #rand_formula = "(1|color_comsint)",
             p_adj_method = "holm",
             prv_cut = 0,
             s0_perc = 0.05 )

m1<-0
head(m1$res)

res_24<-m1$res
res_48<-m1$res
res_72<-m1$res




#se subieron al cluster
write.csv(freq, "C:/Users/natal/Documents/LIIGH/freq.csv")
write.csv(meta, "C:/Users/natal/Documents/LIIGH/meta.csv")



#######################################
#ancom renewed 
#quitar cepas que no pertenezcan a la comunidad
#elevar hrs al cuadrado o hacer nueva variable 
#quitar temp*hrs
#hacer grafica de lo que si es significativo con respecto al tiempo 

NS1_comsints<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet="syncoms")%>%
  as.data.frame()
NS2_comsints<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet="syncoms")%>%
  as.data.frame()


NS_comsints_metadata<-NS2_comsints%>%
  left_join(NS1_comsints, by = "strain")%>%
  tibble::column_to_rownames("strain")%>%
  t()%>%
  as.data.frame()%>%
  tibble::rownames_to_column(var="community")%>%
  rename("NS_042g_27F"="ST00042", "NS_164C_27F"="ST00164", "NS_110C_1_27F"="ST00110")

presence_strains<- melt(NS_comsints_metadata, id="community", variable.name = "strain", value.name = "presence")

p<-presence_strains%>%
  filter(community=="R7" & presence==1)%>%
  pull(strain)

m<-meta%>%
  filter(community=="R7")%>%
  filter(!temp=="NA")%>%
  mutate(hrs_2=hrs*hrs)

f<-freq%>%
  as.data.frame()%>%
  select(any_of(rownames(m)))%>%
  tibble::rownames_to_column(var = "strain")%>%
  filter(strain %in% p)%>%
  tibble::column_to_rownames("strain")%>%
  
  as.matrix()

m2<-ancombc2(data = f,
             taxa_are_rows = TRUE,
             meta_data = m,
             fix_formula = "temp+hrs+hrs_2",
             rand_formula = "(1|exp)",
             p_adj_method = "holm",
             prv_cut = 0,
             s0_perc = 0.05)

m2$res$`q_(Intercept)`
m2$res$q_temp32
m2$res$q_hrs
m2$res$q_hrs_2


m2$res


