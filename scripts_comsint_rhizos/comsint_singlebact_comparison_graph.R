#COMPARA EL CRECIMIENTO DE LAS COMSINTS (OD) CON EL CRECIMIENTO DEL AVERAGE DE LAS SINGLESTRAINS 

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)


#datos de od data NS1 OD real 
#datos de comsints en data NS1 comsints 
#datos de singlebact para singlebacte mean en data test bact y data test single 

#crecimiento de single strains
ss<-read.table(file = 'C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/clean_tables/pilot_strain_growth_curves_filtered.tsv', sep = '\t', header = TRUE)
ss1<-ss%>%
  rename("od600"="OD600")%>%
  select(c("strain", "temp", "timepoint", "od600"))%>%
  filter(!timepoint==c("t_4", "t_5"))%>%
  mutate(time=case_when(
    timepoint=="t_0"~0,
    timepoint=="t_1"~24,
    timepoint=="t_2"~48,
    timepoint=="t_3"~72))%>%
  select(-timepoint)%>%
  group_by(strain, time, temp)%>%
  summarize(across(c("od600"),mean))%>%
  as.data.frame()



#community composition, which strain should be in which community 
ns1 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet = "syncoms")%>%
  as.data.frame()
ns2 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet = "syncoms")%>%
  as.data.frame()%>%
  select(-"strain")

ns<-cbind(ns1,ns2) 

ns<-melt(ns, id.vars = "strain")%>%
  filter(!is.na(value))

data_ss<-merge(ss1,ns, by = "strain")

data_ss <- data_ss %>%
  group_by(temp, time, variable) %>%
  summarize(
    mean_value = mean(od600, na.rm = TRUE),
    sum_value = sum(od600, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()%>%
  rename("Community" = "variable", "od600_sum" = "sum_value", "od600_mean"="mean_value")


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
data_ss$time<-as.factor(data_ss$time)


plot<-ggplot()+
  geom_line(data=data_comsints, aes(x=time, y=od600, group = interaction(temp, exp), colour = as.factor(temp), linetype="Synthetic communities"), size=0.8)+
  geom_line(data=data_ss, aes(x=time, y=od600_mean, group = temp, colour = as.factor(temp), linetype = "Interspecific competition"), size=0.8)+
  geom_line(data=data_ss, aes(x=time, y=od600_sum, group = temp, colour = as.factor(temp), linetype = "Independence"), size=0.8)+
  facet_wrap(~Community)+
  scale_color_manual(values =colors)+
  scale_linetype_manual(values = c("Synthetic communities" = "solid", "Interspecific competition" = "dashed", "Independence"="dotted")) +
  labs(title = "Growth: Individual vs Communities  ", y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), color="Temperature (°C)", linetype="Growth")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))


plot 
ggsave(plot, 
       path="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/graphs/indiv_and_comsint_graphs/",
       filename ="comparison_plot.png",
       width = 30, height = 14, units = "cm")



