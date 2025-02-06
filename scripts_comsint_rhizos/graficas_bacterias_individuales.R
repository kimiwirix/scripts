#grafica de experimetnos bacterias individuales con protocolo comsint  
#eje x: horas transcurridas totales 
#eje y: OD600

library(readODS)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(tidyverse)
library(ggpubr)


#data 1 y 2 son las réplicas 
data_1 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_test_single_strains.ods", sheet = "OD_1")
data_2 <- read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_test_bacteria_comsint.ods", sheet = "OD600")

#cambia colnames de t_0, t_1 .. a 0,24,48...
colnames(data_1)<-c("strain","temp", "0" , "4" ,"20","24","48","72")
colnames(data_2)<-c("strain","temp", "0" , "24","48","72")

#modifica data porque hay timpoints a 4 y 20hrs y cepas ST00122 y ST00021
od600_1 <- data_1 %>%
  as.data.frame() %>%
  select(-"4",-"20") %>%
  filter(!strain %in% c("ST00122","ST00021")) %>%
  mutate(replica=1) 

#MOdifica data 2 y lo une con data 1 por columnas 
od600_2 <- data_2 %>%
  as.data.frame() %>%
  mutate(replica=2) %>%
  full_join(od600_1)

#rearrange dataframe
all_od600<-melt(od600_2, id.vars = c("strain","temp","replica"), 
                value.name = "od600", variable.name = "time")

colors<-c("28"="#63B8FF", "32"="lightsalmon")

plot<-ggplot(all_od600, aes(time, od600, group = interaction(replica, temp), colour = as.factor(temp)))+
  facet_wrap(~strain, nrow = 2)+
  geom_point()+
  geom_line()+
  scale_color_manual(values = colors)+
  labs(title = "Individual growth", y = "OD600", x = "Time (hrs)", color="Temperature (°C)")+
  theme(plot.title = element_text(hjust = 0.5))


plot  
ggsave(plot, 
       path="C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/indiv_and_comsint_graphs/individual_strains/",
       filename ="all_together.png",
       width = 30, height = 14, units = "cm")



