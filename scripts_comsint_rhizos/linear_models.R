#linear models tutorial: https://statsandr.com/blog/multiple-linear-regression-made-simple/#visualizations-1

library(dplyr)
library(pracma)
library(readODS)
library(reshape2)
library(tidyverse)
library(performance)
library (see)
library(car)


#FREQUENCY TABLE 
frequency_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/f_clean.tsv', sep = '\t', header = TRUE, row.names = "row.names")

proportion_table<-apply(frequency_table, 2, function(x) x/sum(x))
proportion_table<-proportion_table %>%   
  t() %>% 
  as.data.frame()
proportion_table$label<-rownames(proportion_table) #pone como primera columna los rownames para mergear con las otras tablas

proportion_table<-proportion_table%>%
  rename("Pseudomonas sp.2"="ST00101", "Agrobacterium sp."="ST00154", "Pseudomonas sp.1"="NS_042g_27F", "Arthrobacter sp."="ST00060", "Bacillus sp.1"="ST00046", "Paenibacillus sp."="ST00143", "Bacillus sp.2"="NS_164C_27F", "Variovorax sp."="NS_110C_1_27F", "Rhodococcus sp."="ST00094", "Mycobacterium sp."="ST00109")


#DATA OF LECTURES ODS PER COMSINTS 
data_comsint_1 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet = "OD600_real") %>%
  mutate(exp="NS1")%>%
  as.data.frame()
data_comsint_2 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet = "OD600_real") %>%
  mutate(exp="NS2")%>%  
  as.data.frame()
data_comsint_3 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS3.ods", sheet = "OD600_real") %>%
  mutate(exp="NS3")%>%  
  as.data.frame()
data_comsint_4 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS4.ods", sheet = "OD600_real") %>%
  mutate(exp="NS4")%>%  
  as.data.frame()

data_comsints_auc<-rbind(data_comsint_1,data_comsint_2,data_comsint_3,data_comsint_4)%>%
  rowwise() %>%
  mutate(
    AUC_OD = trapz(
      x = c(0, 24, 48, 72),
      y = c(t_0, t_1, t_2, t_3))) %>%
  ungroup()
colnames(data_comsints_auc)<-c("community","temp", "0","24","48","72", "exp", "AUC_OD")
comsints_auc<-melt(data_comsints_auc, id.vars=c("community","exp","temp", "AUC_OD"),variable.name = "hrs", value.name = "od600") 




#METADATA 
metadata_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/metadata_clean.tsv', sep = '\t', header = TRUE)
meta<-metadata_table%>%
  select(!c(day, techrep))
sort(metadata_table$label)==sort(colnames(frequency_table))


presence<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/presence_strains_in_comsints.tsv', sep = '\t', header = TRUE)%>%
  mutate(strain = case_when(
    strain == "ST00101" ~ "Pseudomonas sp.2",
    strain =="ST00154" ~  "Agrobacterium sp.",
    strain =="NS_042g_27F" ~  "Pseudomonas sp.1",
    strain =="ST00060" ~  "Arthrobacter sp.",
    strain =="ST00046" ~  "Bacillus sp.1",
    strain =="ST00143" ~  "Paenibacillus sp.",
    strain =="NS_164C_27F" ~  "Bacillus sp.2",
    strain =="NS_110C_1_27F" ~  "Variovorax sp.",
    strain =="ST00094" ~  "Rhodococcus sp.",
    strain =="ST00109" ~  "Mycobacterium sp."))
                         

table_all<-merge(proportion_table, meta, by="label")
table_all<-merge(table_all, comsints_auc, by=c("community", "exp", "temp", "hrs"))
table_all <- melt(table_all, id = c('label', 'AUC_OD','od600', 'hrs', 'community','temp','exp'), variable.name = 'strain', value.name = "proportion")#reorganiza tabla por strains 

table_all<-table_all%>%
  group_by(strain, community, exp, temp) %>%
  mutate(
    AUC_prop = trapz(hrs, proportion)
  )%>%
  ungroup()%>%
  mutate(
    group_exp=case_when(
      exp %in% c("NS1", "NS3") ~1,
      exp %in% c("NS2", "NS4") ~2))%>%
  inner_join(presence, by=c("strain", "community", "exp","temp","hrs","label"))%>%
  group_by(community, strain, temp, exp) %>%
  mutate(
    der_prop = coef(lm(proportion ~ hrs))[["hrs"]],
    der_OD   = coef(lm(od600 ~ hrs))[["hrs"]])%>%
  ungroup()


t <- table_all %>%
  filter(presence==1)


##NEWLY ADDED. THIS GETS RID OF REPETITIVE VALUES AND NOW NOTHING IS SIGNIFICANT:(( 
t<-t %>%
  group_by(strain, AUC_OD, temp, community) %>%
  summarise(der_prop = unique(der_prop),
            .groups = 'drop') 

t$temp<-as.factor(t$temp)  
t$community<-as.factor(t$community) 


#Table with values from best fitted model
strains <- c("Bacillus sp.1","Agrobacterium sp.", "Pseudomonas sp.2", "Mycobacterium sp.", "Pseudomonas sp.1", "Paenibacillus sp.", "Bacillus sp.2", "Rhodococcus sp." ,"Variovorax sp.","Arthrobacter sp.")
results<-data.frame()


for (strain_name in strains) {
  m<-lm(AUC_OD~community+der_prop*temp, data = t%>%
          filter(strain==strain_name))
  s<-summary(m)
  
  results <- rbind(results, data.frame(
    strain = strain_name,
    bacterial_slope = s$coefficients["der_prop", "Estimate"],
    der_prop_pval = s$coefficients["der_prop","Pr(>|t|)"],
    temperature = s$coefficients["temp32","Estimate"],
    temp_pval = s$coefficients["temp32","Pr(>|t|)"],
    slope_temp_int = s$coefficients["der_prop:temp32","Estimate"],
    int_pval = s$coefficients["der_prop:temp32","Pr(>|t|)"],
    r_sq = s$r.squared
  ))
}


#heatmap
r<-results%>%
  rowwise() %>%
  mutate(
    bacterial_slope =case_when(
      der_prop_pval > 0.05 ~ NA_real_,
      .default = bacterial_slope  ),
    temperature =case_when(
      temp_pval > 0.05 ~ NA_real_,
      .default = temperature ),
  slope_temp_int=case_when(
    int_pval > 0.05 ~ NA_real_,
    .default = slope_temp_int))%>%
  select(c(strain,bacterial_slope,temperature,slope_temp_int))%>%
  pivot_longer(cols = c(bacterial_slope,temperature,slope_temp_int),
               names_to = "coefficient",
               values_to = "value")  %>%
  mutate(
    effect=case_when(
      value >0 ~ "Positive",
      value <0 ~ "Negative", 
      is.na(value) ~ "Not significant"
    )
  )

r$strain<- factor(r$strain, levels=strains)#acomoda

heatmap<-ggplot(r, aes(x = coefficient, y = strain, fill = effect)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("#EE2C2C", "mistyrose4", "deepskyblue2"))+
  labs(title = "Effect on community performance ",
       fill = "Effect    ",
       x = "Coefficient",
       y = "Strain")+
  geom_text(aes(label = round(value, 3)), color = "black", size = 3 )+
  theme(legend.position="bottom",
        legend.justification = c(2.2, 2))+
  theme(axis.text.y = element_text(face="italic"))


heatmap
ggsave(heatmap,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/heatmap_coeff_effects.png" ,
       bg="white",  width = 12, height = 16, units = "cm")




#para cada muestra convertir porcentaje por el od (der prop por od) y hacer una pendiente por cepa por temp por comsint 
#mandar lista de 4c 
hacer heatmap por dia para ver si hay efectos de las bacterias en un dia si y luego no y asi 


