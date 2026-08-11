#graficas growth curves individuales en temps 30 y 37°C 
#shakers info S_1(small izq); S_2(small der); B_1(big up); B_2(big down)

library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(ggtext)



t <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_filtered.tsv", header = TRUE,  sep='\t' )


real_names<-c('CH23'='<i>Bacillus altitudinis</i>', 'CH29'='<i>Corynebacterium sp.</i>', 'CH90'='<i>Bacillus atrophaeus</i>', 'CH99b'='<i>Staphylococcus arlettae</i>', 'CH111'='<i>Bacillus thuringiensis</i>', 'CH149a'='<i>Micrococcus luteus</i>', 'CH154a'='<i>Staphylococcus shinii</i>', 'CH161d'='<i>Bacillus infantis</i>',  'CH447'='<i>Priestia megaterium</i>', 'CH450'='<i>Metabacillus indicus</i>')
custom_colors <- c("CH23"="#273a3eff", "CH29"="#08519cff",  "CH99b"="#00e5eeff", "CH111"="#4682b4ff",  "CH154a"="#bdd7e7ff", "CH90"="#8c2424ff", "CH149a"="#cd2626ff", "CH161d"="#ff0000ff", "CH447"="#ef6d53ff", "CH450"="#fcae91ff" )


#GRAPHS
#wrapped por temp

g1<-ggplot(data = t, aes(x = hr, y = `OD.real`, colour = Cepa, group = interaction(Cepa,rep, temp, drop = TRUE, sep = "_")))+
  geom_line()+
  facet_wrap(~temp)+
  #scale_color_hue(labels = real_names)+
  scale_x_continuous(breaks = seq(0,18, by=2))+
  labs( title = "Growth curves per temperature",y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), colour="Strain")+
  geom_vline(xintercept = 12, colour = "red", linetype = "dashed") + 
  theme(legend.text = element_markdown(),
        plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) +   # enables <i></i> formatting
  scale_color_manual(values = custom_colors ,
                     breaks = names(real_names),
                     labels = real_names)  # change colors here

g1


#wrapped por cepa 
g2<-ggplot(data=t, aes(x = hr, y = `OD.real`, colour = as.factor(temp), group = interaction(rep, temp, drop = TRUE, sep = "_")))+
  geom_line()+
  facet_wrap(~Cepa, scales = "free_y", ncol=5,labeller = as_labeller(real_names))+
  scale_x_continuous(breaks = seq(0,18, by=2))+
  labs( title = "Growth curves by strain",y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), colour="T(°C)")+
  geom_vline(xintercept = 12, colour = "red", linetype = "dashed")+
  scale_color_manual(
    values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))+  # change colors here
  theme(strip.text = element_markdown(),                                               
        plot.title = element_text(hjust = 0.5, vjust = 3, size = 12))    # enables <i></i> formatting

g2


#one plot distinguishing between temps
g3<-ggplot(data = t, aes(x = hr, y = `OD.real`, colour = as.factor(temp),  group = interaction(Cepa,temp,rep)))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,18, by=2))+
  labs( title = "Growth curves by temperature",y=expression("OD"["600nm"]), x = expression("Time"["hrs"]), colour="T(°C)")+
  geom_vline(xintercept = 12, colour = "red", linetype = "dashed")+
  scale_color_manual(
    values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3"))  # change colors here
g3


ggsave(g1,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/gc_and_rxnnorm/individual_growth_curve.png" ,
       bg="white",  width = 30, height = 14, units = "cm")
ggsave(g2,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/gc_and_rxnnorm/individual_growth_curve_bystrain.png" ,
       bg="white",  width = 30, height = 14, units = "cm")
ggsave(g3,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/gc_and_rxnnorm/individual_growth_curve_bytemp.png" ,
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
