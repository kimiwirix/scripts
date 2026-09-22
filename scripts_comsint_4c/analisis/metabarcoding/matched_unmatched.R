#+ Calculates from all the comsints the percentage of frequencies corresponding 
#+ to unmatched and of matched the calculation was done for 99 perc indetity
#+ without primers the proportion of unmatched decreases

library(ggplot2)
library(readODS)
library(dplyr)
library(tidyverse)

strain_ids<-c("CH111","CH90","CH161d", "CH149a","CH29","CH99b","CH154a","CH23","CH447", "CH450")

frequency_table <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/feature-table-open-nonchimeric.tsv", 
                            sep = "\t",
                            header = TRUE) 


#+ mutate saca proportions 
#+ agrega otra columna llamada status los que si matchearon y las que no 

f<-frequency_table%>%
  mutate(across(-strain, ~ .x / sum(.x)))%>%                                    
  mutate(status = ifelse(strain %in% strain_ids,                                
                         "matched","unmatched"))%>%
  pivot_longer(
    cols = -c(strain, status),
    names_to = "community_label",
    values_to = "freq"
  )%>%
  group_by(community_label, status) %>%
  summarise(total_frequency = sum(freq), .groups = "drop")
  




plot<-ggplot(data = f, aes(x = community_label, y = total_frequency, fill = status))+
  geom_bar(position = "stack", stat = "identity")+
  labs(title = "Reads",
       y = "Frequency", x = "Community", fill="status") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

plot
ggsave(plot,
       filename="C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/matched_unmatched.png" ,
       bg="white",  width = 30, height = 14, units = "cm")



