#compares absolute frequencies (realtive freq*MPN) with od600 data from thursday 
library(readODS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggpubr)


#MPN data
MPN_sheet<-read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/proportion_metadata_99perc_identity_MPN.ods", sheet = "MPN")%>%
  as.data.frame()%>%
  drop_na()%>%
  mutate_at("hrs", as.factor) #temp in proportion_sheet is as character, not as numeric due to NAs 

#qPCR data
qpcr_sheet<- read_ods("C:/Users/natal/Documents/LIIGH/results/results_comsint_rhizos/proportion_metadata_99perc_identity_MPN.ods", sheet = "qPCR")%>%
  as.data.frame()

#ODs data
od_ns1<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet="OD600_real")%>%
  as.data.frame()%>%
  mutate(exp="NS1")
od_ns2<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet="OD600_real")%>%
  as.data.frame()%>%
  mutate(exp="NS2")
od_ns3<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS3.ods", sheet="OD600_real")%>%
  as.data.frame()%>%
  mutate(exp="NS3")
od_ns4<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS4.ods", sheet="OD600_real")%>%
  as.data.frame()%>%
  mutate(exp="NS4")

od_ns<-rbind(od_ns1,od_ns2,od_ns3,od_ns4)%>%
  rename("0"="t_0", "24"="t_1", "48"="t_2", "72"="t_3", "community"="Community")



melted_od_ns<-melt(od_ns, id = c("community", "temp", "exp"), variable.name = "hrs", value.name = "od_600")
#All data in one df 
od_mpn_qpcr<-melted_od_ns%>%
  inner_join(MPN_sheet)%>%
  left_join(qpcr_sheet, by = "label")



ggplot(od_mpn_qpcr, aes(x=OD600, y = `Fragmentos Totales / mL`))+
  geom_point()+
  facet_wrap(~exp)+
  geom_smooth(method = "lm")+
  scale_y_log10()+
  stat_regline_equation()

  stat_cor(aes(x = OD600, y = `Fragmentos Totales / mL`, label = after_stat(rr.label)))




