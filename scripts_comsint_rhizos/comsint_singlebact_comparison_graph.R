#COMPARA EL CRECIMIENTO DE LAS COMSINTS (OD) CON EL CRECIMIENTO DEL AVERAGE DE LAS SINGLESTRAINS 

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)


#datos de od data NS1 OD real 
#datos de comsints en data NS1 comsints 
#datos de singlebact para singlebacte mean en data test bact y data test single 

#data 1 y 2 son las réplicas de crecimiento de bact indiviiduales 
data_ss_1 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_test_single_strains.ods", sheet = "OD_1") %>%
  as.data.frame()%>%
  rename("0"="t_0", "4"="t_1", "20"="t_2", "24"="t_3", "48"="t_4", "72"="t_5")%>%
  select(-"4",-"20") %>%
  filter(!strain %in% c("ST00122","ST00021")) 

data_ss_2 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_test_bacteria_comsint.ods", sheet = "OD600")%>%
  rename("0"="t_0", "24"="t_1", "48"="t_2", "72"="t_3")%>%
  full_join(data_ss_1)%>%
  group_by(strain, temp)%>%
  summarize(across(c("0","24","48","72"), mean))%>%
  as.data.frame()

data_ss<-melt(data = data_ss_2, id.vars = c("strain", "temp"), variable.name = "time", value.name = "od600")


###community composition, which strain should be in which community 
ns1 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet = "syncoms")%>%
  as.data.frame()
ns2 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet = "syncoms")%>%
  as.data.frame()%>%
  select(-"strain")

ns<-cbind(ns1,ns2) 

ns<-melt(ns, id.vars = "strain")%>%
  filter(!is.na(value))

data_ss<-merge(data_ss,ns, by = "strain")

data_ss<-data_ss%>%
  group_by(temp,time,variable)%>%
  summarize(mean_value = mean(od600, na.rm = TRUE))%>%
  as.data.frame()%>%
  rename("Community"="variable", "od600"="mean_value")



#data ods per comsint 
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


data_comsints<-rbind(data_comsint_1,data_comsint_2,data_comsint_3,data_comsint_4)
colnames(data_comsints)<-c("Community","temp", "0","24","48","72", "exp")


data_comsints<-melt(data_comsints, id.vars=c("Community","exp","temp"), 
                    variable.name = "time", value.name = "od600") 




#PLOT
communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
colors<-c("28"="#63B8FF", "32"="lightsalmon")


data_comsints$Community <- factor(data_comsints$Community, levels = communities) # Custom order
data_ss$Community <- factor(data_ss$Community, levels = communities) # Custom order


plot<-ggplot(NULL, aes(x = time, y = od600))+
  geom_line(data=data_comsints, aes(group = interaction(temp, exp), colour = as.factor(temp), linetype="Synthetic communities"))+
  geom_line(data=data_ss, aes(group = temp, colour = as.factor(temp), linetype = "Single strains"))+
  facet_wrap(~Community)+
  scale_color_manual(values =colors)+
  scale_linetype_manual(values = c("Synthetic communities" = "solid", "Single strains" = "dashed")) +
  labs(title = "Growth: Individual vs Communities  ", y = "OD600", x = "Time (hrs)", color="Temperature (°C)", linetype="Growth")+
  theme(plot.title = element_text(hjust = 0.5))



plot  
ggsave(plot, 
       path="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/indiv_and_comsint_graphs/",
       filename ="comparison_plot.png",
       width = 30, height = 14, units = "cm")











