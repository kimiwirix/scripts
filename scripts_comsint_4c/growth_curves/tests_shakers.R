#Test of the shakers only on strain CH447 to come up with equivalencias in rpms between small and big 

library(readODS)
library(dplyr)
library(ggplot2)
library(pracma)
library(performance)


#TESTS SHAKERS
t<-read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW_filtered.tsv", 
              sep = '\t', 
              header = TRUE)
data_shakers <-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW.ods", sheet = "shakers_exp_2")
data_shakers$`OD real`[data_shakers$`OD real` == 0.620 & data_shakers$Incubator=="S_2"] <- NA #remove 


#plot of the growth when ch447 was grown in small at 150rpms and in big at 125rpms
ggplot()+
  geom_line(data = data_shakers, aes(x=hr, y=`OD real`, colour=Incubator,group = interaction( Incubator, rep, temp, drop = TRUE, sep = "_"), linetype = "shakers"))+
  geom_line(data = t%>%
              filter(Cepa=="CH447" & temp==37), aes(x=hr, y=`OD.real`, colour=Incubator, group = interaction(Cepa,rep,temp, drop = TRUE, sep="_"), linetype = "CH447"))+
  labs( linetype="experiment")



s<-data_shakers%>%
  group_by(Incubator, rep)%>%
  mutate(AUC=trapz(hr,`OD real`))


#replicated rows in AUC influence the results since n is higher 
s %>%
  group_by(Cepa, rep, temp, Incubator) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop') %>%
  aov(AUC ~ Incubator, data = .) -> m1.incubator

summary(m1.incubator)


#conclusions: When shakers are homogeneized at 125rpms (big) and 150rpms (small) there is no statistically signoficance in growth differences 







