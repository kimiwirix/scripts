#calculates absolute frequencies based on the MPN results 
#we only have MPN for hrs:48 (jueves)
library(readODS)
library(dplyr)
library(tidyr)
library(ggplot2)

proportion_sheet<-read_ods("C:/Users/natal/Documents/LIIGH/proportion_metadata_99perc_identity_MPN.ods", sheet = "proportion_metadata_99perc_identity")%>%
  as.data.frame()%>%
  select(!"...1")

MPN_sheet<-read_ods("C:/Users/natal/Documents/LIIGH/proportion_metadata_99perc_identity_MPN.ods", sheet = "MPN")%>%
  as.data.frame()%>%
  drop_na()%>%
  mutate_at("temp", as.character) #temp in proportion_sheet is as character, not as numeric due to NAs 

strains<-c("ST00046", "ST00154", "ST00101" ,"ST00109", "NS_042g_27F","ST00143" , "NS_164C_27F" , "ST00094" ,"NS_110C_1_27F" , "ST00060" )

prop_MPN<-inner_join(proportion_sheet, MPN_sheet, by = c("community", "temp", "exp", "hrs"))%>%
  mutate(across(all_of(strains), function(x) x* `CFU/ml` ))

melted_prop_MPN<-melt(prop_MPN, id = c("label", "community", "hrs", "temp", "techrep", "exp", "CFU/ml"), variable.name = "strain")



communities<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","R11","R12")
custom_colors <- c("ST00046" = "#25383C", "ST00154" = "#08519C", "ST00101" = "#00E5EE","ST00109" = "#4682B4", "NS_042g_27F" = "#BDD7E7",
                   "ST00143" = "#8B2323", "NS_164C_27F" = "#CD2626", "ST00094" = "#FF3030","NS_110C_1_27F" = "#EE6A50", "ST00060" = "#FCAE91")


melted_prop_MPN<-melted_prop_MPN[melted_prop_MPN$exp=="NS1"|melted_prop_MPN$exp=="NS3",]
#melted_prop_MPN<-melted_prop_MPN[melted_prop_MPN$exp=="NS2"|melted_prop_MPN$exp=="NS4",]
melted_prop_MPN$community <- factor(melted_prop_MPN$community, levels = communities) # Custom order




ggplot(data = melted_prop_MPN, aes(x = hrs, y = value, fill = strain))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = custom_colors)+
  facet_wrap(~community+temp+exp, ncol = 6, scales = "free_y")+
  scale_x_continuous(breaks = 48)+
  labs(title = "Community composition through time and temperature",
       y = "Abundance", x = "Time (hrs)", fill="Strain") + 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

