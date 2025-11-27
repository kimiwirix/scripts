library(vegan)
library(readODS)
library(dplyr)
library(tidyr)
library(textshape)


#frequency table
frequency_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/f_clean.tsv', sep = '\t', header = TRUE, row.names = "row.names")

freq<-frequency_table %>%   
  t() %>% 
  as.data.frame()


#METADATA
metadata_table<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/metadata_clean.tsv', sep = '\t', header = TRUE)
sort(metadata_table$label)==sort(colnames(frequency_table))

meta<-metadata_table


###div analysis 
#sh:if diverse thf uncertainty of knowing species of random indiv is high 
#even:  how close in numbers each species in an environment is. 1 very even 

shannon<-diversity(freq)%>%
  as.data.frame()%>%
  rename("."="i_shannon")%>%
  tibble::rownames_to_column(var = "label")

evenness<-(shannon$i_shannon/log(specnumber(freq)))%>%
  as.data.frame()%>%
  rename("."="i_evenness")%>%
  tibble::rownames_to_column(var = "label")


richness<-(specnumber(freq))%>%
  as.data.frame()%>%
  rename("."="i_richness")%>%
  tibble::rownames_to_column(var = "label")



##METADATA TRANSFROMATION
#df_NAs duplica las filas que tienen NA de temp, y a una le pone 28 y a otra 32 para que aparezcan dentro del plot en tiempo 0
df_NAs <- meta %>%
  filter(is.na(temp))%>%
  uncount(2) %>%
  mutate(temp = rep(c("28", "32"), length.out = n())) 

meta2<-meta%>%
  filter(temp != "NA")%>%
  rbind(df_NAs)%>%
  left_join(shannon, by = "label")%>%
  left_join(evenness, by = "label")%>%
  left_join(richness, by = "label")%>%
  mutate(rep= case_when(
    exp %in% c("NS1", "NS2") ~ "rep1",
    exp %in% c("NS3", "NS4") ~ "rep2"
  ))


communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
colors<-c("28"="#63B8FF", "32"="lightsalmon")
meta2$community <- factor(meta2$community, levels = communities) # Custom order




#PLOTS 
p_shannon<-ggplot(meta2)+
  geom_line(aes(x = hrs, y=i_shannon, group=interaction(temp,exp), colour = as.factor(temp), linetype=as.factor(rep)), linewidth=1)+
  facet_wrap(~community)+
  scale_x_continuous(breaks = c(0, 24, 48,72))+
  labs(title = "Shannon diversity Index",x="Time (hrs)", y="Value", colour="Index", linetype="Replicates" )+
  scale_color_manual(values = colors)

p_evenness<-ggplot(meta2)+
  geom_line(aes(x = hrs, y=i_evenness, group=interaction(temp,exp), colour = as.factor(temp), linetype=as.factor(rep)), size=1)+
  facet_wrap(~community)+
  scale_x_continuous(breaks = c(0, 24, 48,72))+
  labs(title = "Pielou's evenness Index",x="Time (hrs)", y="Value", colour="Index", linetype="Replicates" )+
  scale_color_manual(values = colors)

p_richness<-ggplot(meta2)+
  geom_line(aes(x = hrs, y=i_richness, group=interaction(temp,exp), colour = as.factor(temp), linetype=as.factor(rep)), size=1)+
  facet_wrap(~community)+
  scale_x_continuous(breaks = c(0, 24, 48,72))+
  labs(title = "Richness Index",x="Time (hrs)", y="Value", colour="Index", linetype="Replicates" )+
  scale_color_manual(values = colors)




ggsave(p_shannon,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/diversity_analysis/p_shannon.png" ,
       bg="white",  width = 30, height = 14, units = "cm")
ggsave(p_evenness,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/diversity_analysis/p_evenness.png" ,
       bg="white",  width = 30, height = 14, units = "cm")
ggsave(p_richness,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/diversity_analysis/p_richness.png" ,
       bg="white",  width = 30, height = 14, units = "cm")

https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf 
https://rpubs.com/an-bui/vegan-cheat-sheet 







ggplot(meta2)+
  geom_line(aes(x = hrs, y=i_shannon, group=interaction(community,exp), colour = community, linetype=as.factor(rep)), linewidth=1)+
  facet_wrap(~temp)+
  scale_x_continuous(breaks = c(0, 24, 48,72))+
  labs(title = "Shannon diversity Index",x="Time (hrs)", y="Value", colour="Index", linetype="Replicates" )



meta2
m<-lm(i_shannon~as.factor(temp)*as.factor(hrs), data = meta2)
summary(m)
AIC(m)
