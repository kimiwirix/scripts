#stat analysis to see if the temperature has a significant effect on the individual growth 
#conclusiones at the end 

library(dplyr)
library(ggplot2)
library(pracma)
library(performance)


t<-read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW_filtered.tsv", 
           sep = '\t', 
           header = TRUE)


#AUC addition and eliminate filas que se repitan para no afectar significancia
t_AUC<-t%>%
  group_by(Cepa, rep, temp)%>%
  mutate(AUC=trapz(hr,`OD.real`))%>%
  ungroup()%>%
  group_by(Cepa, rep, temp, Incubator) %>%
  summarise(AUC = unique(AUC),
            .groups = 'drop')


t_AUC$temp<-as.factor(t_AUC$temp)  

strains <- c("CH23","CH29" ,"CH90", "CH99", "CH111", "CH149a", "CH154a", "CH161d" ,"CH447","CH450")
results<-data.frame()


for (strain_name in strains) {
  m<-lm(AUC~temp, data = t_AUC%>%
          filter(Cepa==strain_name))
  s<-summary(m)
  anova_overall<- summary(aov(m))
  
  results <- rbind(results, data.frame(
    strain = strain_name,
    temp_37 = s$coefficients["temp37","Estimate"],
    pval_37 = s$coefficients["temp37","Pr(>|t|)"],
    temp_42 = s$coefficients["temp42","Estimate"],
    pval_42 = s$coefficients["temp42","Pr(>|t|)"],
    pval_anova = anova_overall[[1]]$`Pr(>F)`[1] #to see the overall effect of the categ variable (temp with the tree levels) on the AUC 
  ))
}

t_AUC
results

#Few (3) have a positive effect (signifficantly) from 30 to 37 
#More (6) have different effects (signifficantly) when compared from 30 to 42
#Overall (anova) temperature has a significant role in the growth of (8) strains 






