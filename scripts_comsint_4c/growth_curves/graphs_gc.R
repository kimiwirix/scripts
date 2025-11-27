#graficas growth curves individuales en temps 30 y 37°C 
#shakers info S_1(small izq); S_2(small der); B_1(big up); B_2(big down)

library(readODS)
library(dplyr)
library(ggplot2)
library(pracma)
library(performance)



file<-"C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW.ods"
sheets<-ods_sheets("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW.ods")
ch<-sheets[grepl("^CH", sheets)]



t<-lapply(ch, function(ch_sheet){ #all sheets that start with CH in one tibble  
  read_ods(file, sheet = ch_sheet)
})%>%
  bind_rows()%>%
  filter(!is.na(`OD real`)) %>% #filters rows I put as separation between temps in ods 
  filter(!is.na(`fecha`))



t<-t%>% #Remove due to shakers inconsistencies and in case of CH29 due to contamination 
  filter(!(
    rep == 1 & Incubator =="B_2" |
      rep == 2 & Incubator =="B_2" |
      rep == 3 & Incubator =="B_2" |
      Cepa == "CH29" & rep == 2 & temp == 30|
      Cepa == "CH29" & rep == 2 & temp == 42|
      Cepa == "CH29" & rep == 5 & temp == 37))

head(t)

#export metadata info to upload in cluster shared_data
#ultimate filtered table with the useful data 
write.table(t, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW_filtered.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = FALSE)

#GRAPHS
#wrapped por temp
g1<-ggplot(data = t, aes(x = hr, y = `OD real`, colour = Cepa, group = interaction(Cepa,rep, temp, drop = TRUE, sep = "_")))+
  geom_line()+
  facet_wrap(~temp)+
  scale_x_continuous(breaks = seq(0,18, by=2))+
  labs( title = "Growth curve",y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), colour="Strain")+
  geom_vline(xintercept = 8, colour = "red", linetype = "dashed")
g1

#wrapped por cepa 
g2<-ggplot(data=t, aes(x = hr, y = `OD real`, colour = as.factor(temp), group = interaction(rep, temp, drop = TRUE, sep = "_")))+
  geom_line()+
  facet_wrap(~Cepa, scales = "free_y")+
  scale_x_continuous(breaks = seq(0,18, by=2))+
  labs( title = "Growth by strain",y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), colour="T(°C)")+
  geom_vline(xintercept = 8, colour = "red", linetype = "dashed")+
  scale_color_manual(
    values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))  # change colors here
g2


#one plot distinguishing between temps
g3<-ggplot(data = t, aes(x = hr, y = `OD real`, colour = as.factor(temp),  group = interaction(Cepa,temp,rep)))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,18, by=2))+
  labs( title = "Growth by temperature",y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), colour="T(°C)")+
  geom_vline(xintercept = 8, colour = "red", linetype = "dashed")+
  scale_color_manual(
    values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))  # change colors here
g3


ggsave(g1,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/individual_growth_curve.png" ,
       bg="white",  width = 30, height = 14, units = "cm")
ggsave(g2,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/individual_growth_curve_bystrain.png" ,
       bg="white",  width = 30, height = 14, units = "cm")
ggsave(g3,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/individual_growth_curve_bytemp.png" ,
       bg="white",  width = 30, height = 14, units = "cm")








#Format for logaritmic analysis SUR 
l<-t%>%
  select(Cepa, temp, `OD real`, `factor diln`, hr, time, fecha, rep)%>%
  rename('Strain'="Cepa", 'od_factor'='factor diln', 'OD600'='OD real', 'total_time_h'='hr', 'timestamp'='time', 'date'='fecha', 'batch'='rep')
write.table(l, 
            file='C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/indiv_growth_curve.tsv', 
            quote=FALSE, 
            sep='\t', 
            row.names = FALSE)
