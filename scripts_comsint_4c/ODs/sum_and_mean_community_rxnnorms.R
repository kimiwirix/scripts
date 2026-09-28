#+ Suma y promedio de las rxn norms comparadas con las rexn norms reales, sobre 
#+ los valores de AUC estandarizados.

library(ggplot2)
library(dplyr)


#+ AUC standardized OD values of comsints and adding the column community_num 
#+ for the community colors in the plot 

auc_comm <- read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/AUC_z_score_comsint.tsv", header = TRUE, sep = '\t') %>%
  mutate(community_num= as.numeric(sub("C", "", community)) )




#+ AUC standardized OD values of the individual strains and because there are 
#+ multiples replicates within each strain and temperature, the mean_reps 
#+ column takes the mean of the reppicates for each strain in each temp. 
  
auc_indiv <- read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/AUC_z_score_indivstrains.tsv", header = TRUE, sep = '\t') %>%
  group_by(Cepa, temp) %>%
  summarise(mean_reps = mean(AUC_z_score), .groups = 'drop')




#+ presence filtered by what it is in every community, and each row was repeated 
#+ three times for temperature inclusion 
p <- read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv", header = TRUE, sep = '\t') %>%
  filter(presence==1) %>% 
  slice(rep(1:n(), each = 3)) %>%
  mutate(temp = rep(c(30, 37, 42), times = n() / 3))




#+ Joined presence with the individual mean between replicates AUC, and the 
#+ addition of a column with the AUC of the strains present in a community 
#+ summed and meaned
a <- p %>%
  left_join(auc_indiv, by = c("real_name"="Cepa", "temp")) %>%
  group_by(community, temp) %>%
  summarise(
    suma_auc = sum(mean_reps, na.rm = TRUE),
    mean_auc = mean(mean_reps, na.rm = TRUE),
    .groups = "drop")
  





#+ order communities by community number and plot 
communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")

auc_comm <- auc_comm %>%
  mutate(community = factor(community, levels = communities))
a <- a %>%
  mutate(community = factor(community, levels = communities))




p1 <- ggplot()+
  geom_point(data = auc_comm, aes(x=temp, y=AUC_z_score, group = repbio), colour="peachpuff4")+
  geom_line(data= auc_comm, aes(x=temp, y=AUC_z_score, group = repbio, colour=community_num), linewidth=1.5)+
  scale_color_gradient(low = "blue", high = "red") +
  
  geom_line(data = a, aes(x=temp, y=suma_auc, linetype = 'Sum'), colour="#698B22", linewidth= 1.2)+
  geom_line(data = a, aes(x=temp, y=mean_auc, linetype = 'Mean'), colour="black" , linewidth= 1.2)+
  
  
  facet_wrap(~community, ncol=8)+
  scale_x_continuous(breaks=c(30,37,42))+
  scale_y_continuous(limits= c(-5,5), breaks = seq(-5,5, by= 2))+
  labs( title = "Individual strains' calculated sum and mean",y=expression("AUC"), x = expression("Temperature °C"), color="Communities", linetype= "Individual strains")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) 




ggsave(plot = p1,
       filename = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/ODs/sum_and_mean_comm_rxnnorms.png",
       bg="white",  width = 38, height = 20, units = "cm") 


