#+ I need to do a linear model that predicts the AUC values of the comunities 
#+ based on the auc values of the strains that make up that community.
#+ 
#+ CONCLUSION: there are rxn norms of some communities cuyo predicted values are 
#+ similar to the measured values. SEE last plot 

 
library(tidyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)


#+ individual and community tables with the Z standardized AUCs and the presence/
#+ absence df. What I want to do is a df that tells me the z_score_standardized 
#+ AUC of every community in each replicate, in each temp (9 values pero community), 
#+ and for each of those rows, I want to know which strain is present in the 
#+ community and what is the AUC of that strain in that community in that temp.

indiv_AUC_z <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/AUC_z_score_indivstrains.tsv", header = TRUE, sep = '\t')
comsint_AUC_z <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/AUC_z_score_comsint.tsv", header = TRUE, sep = '\t')

p <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv", header = TRUE, sep = '\t') %>%
  select(!strain) 




#+ For each individual strain I have the AUCs of the 3 temps in multiple 
#+ replicates. I want to have one value from each strain in each temperature, 
#+ thf do mean

i <- indiv_AUC_z %>%
  group_by(Cepa, temp) %>%
  summarise(mean_AUC = mean(AUC_z_score), .groups = 'drop') 




#+ To the presence df (nrows=10 strains per 32 comms=320rows) I will append the i 
#+ df (avg of auc for 3 temps for each strain =30), so I get the presence of 
#+ each strain in a community 3 times bc of temp, thf 320*3 rows. Then, by 
#+ multiplying presence * mean I get a column where only the AUC of the strains 
#+ present in each comm in each temp is not 0 and take the real value.
#+ The final a df shows the strains present in every temperature in a community 
#+ with their respective mean_AUC (of the strain)

strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Staphylococcus arlettae", "Bacillus thuringiensis", "Staphylococcus shinii", "Bacillus atrophaeus",  "Micrococcus luteus",  "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")


a <- p %>%
  left_join(i, by = c("real_name"="Cepa")) %>%
  mutate(real_name = str_replace_all(real_name, " ", "_")) %>%
  mutate(m_AUC_presence = presence * mean_AUC) %>%
  select(!c(presence, mean_AUC)) %>%
  pivot_wider(names_from = real_name, values_from =c(m_AUC_presence))



#+ c df contains all info regarding comms AUC per temp and replicate (9 rows 
#+ per comm) and the strains that make up that comm and their averaged AUC 


c <- comsint_AUC_z %>%
  left_join(a, by = c('community', 'temp'))



#+ When I run the lm for all of the communties one at a time (prueba) it has 
#+ 2 problems: it produces NAs even when the strains are present and it produces 
#+ negative coefficients for some strains. 
#+ 
#+ This is what the model says: 
#+ 
#+ norma_com = coeff1*norma_cepa1+ coeff2*norma_cepa2 + coeff3*norma_cepa3 + coeff4*norma_cepa4 + coeff5*norma_cepa5
#+ 
#+ no tiene sentodo que los coefs sean negativos porque eso significaria que a 
#+ la norma de rxn de com se le deberia restar la norma de rxn de la cepa indiv 
#+ y eso no tiene sentido plt hay que encontrar modelo que fuerze a que todos 
#+ los ceof sean positivos
#+ 
prueba <- c %>%
  filter(community=='C32')


m<-lm(AUC_z_score  ~ 0 + Bacillus_atrophaeus + Micrococcus_luteus + Bacillus_infantis + 
        Priestia_megaterium + Metabacillus_indicus + Bacillus_altitudinis + Corynebacterium_sp. +
        Staphylococcus_arlettae + Bacillus_thuringiensis + Staphylococcus_shinii , data = prueba )
summary(m)
anova(m)



#+ lo que queremos es que el modelo trate de explicar la norma de rxn de 
#+ comunidad forzando a que todos los coefs sean positivos o 0 plt usar glmnet y 
#+ set lower.limits a 0
#+ 
#+ the for loop loops through all the communities and filters the community in 
#+ f and selects out all the columns that add to 0, bc those are the columns of 
#+ the strains not present in that community.
#+ Then selects the cols of the AUC of the strains (X) and the col of the comm AUC
#+ (Y) and searches from many values of lambda for the best to use in m1.best. 
#+ Finally it stores all the coefficients under the name of the community as a 
#+ df in a list and outside it acomoda todos en un df global (coeff_a)   

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")
coefficients_all <- list()

for (comsint in communities) {
  
  f<-c %>%
    filter(community == comsint) %>%
    select(community, repbio, temp, AUC, AUC_z_score,
           where(~ !all(. == 0)))

  
  X  <- as.matrix(f[,6:10])
  Y <- as.matrix(f[,"AUC_z_score"])
  
  #tests for lambda
  lambda_seq <- 10^seq(2, -2, by = -.1)
  m1 <- cv.glmnet(x = X, y = Y, alpha = 0, lambda = lambda_seq, lower.limits = 0)
  best_lambda <- m1$lambda.min
  best_lambda
  
  
  #actual model
  m1.best <- glmnet(X, Y, alpha = 0, lambda = best_lambda, lower.limits = 0)
  coeffs <- as.matrix(coef(m1.best))[1:6,1]

  
  #stores coefficients
  coefficients_all[[comsint]] <- data.frame(
    community = comsint,
    strains = names(coeffs),
    coefficient = coeffs, 
    intercept = coeffs[1])
  
  
}


coeffs_a <- bind_rows(coefficients_all) %>%
  remove_rownames() %>%
  filter(!strains == '(Intercept)')


#+ este df trae otra vez los datos de las comunidades, por cada comsint hay: 
#+ tres replicas y por cada rep 3 temps y por cada una de esas mediciones hay 
#+ 10 cepas, de las cuales solo 5 pertenecen a cada comunidad (filter) plt 
#+ (nrows 32*3*3*5). 
#+ Al df se le appendea los datos de los coefficientes de cada cepa en cada 
#+ comunidad (32*5) y se multiplican los coefficientes de cada cepa por el AUC 
#+ de cada cepa en cada comunidad (BX) siguiendo el modelo lineal
#+ 
#+ y = Intercept + B1X1 + B2X2 + B3X3 ... + e

comms_info<-c %>%
  pivot_longer(cols= !c('community', 'repbio', 'temp', 'AUC', 'AUC_z_score'), names_to = "strains", values_to = "AUC_z_strains") %>%
  filter(!AUC_z_strains==0) %>%
  left_join(coeffs_a, by = c('community', 'strains')) %>%
  mutate(BX=AUC_z_strains*coefficient)



#+ Este df toma el anterior pero suma todos los BX de las cepas de cada 
#+ comunidad*temp*rep y le suma el intercepto (i_plus_BX_equals_y) este dato ya
#+ es la predicción del AUC de la comunidad basado en los AUCs de las bacterias 
#+ individuales 

comms_summed <- comms_info %>%
  group_by(community, repbio, temp) %>%
  summarise(
    intercept = unique(intercept),
    AUC=unique(AUC), 
    AUC_z_score=unique(AUC_z_score), 
    BX_sum = sum(BX), 
    i_plus_BX_equals_y = BX_sum + intercept
  )
  
  



#+ grafica que muestra la relación entre el AUC de la comunidad real VS el AUC 
#+ de la comunidad predicho con el modelo: 
#+ 
#+ y = Intercept + B1X1 + B2X2 + B3X3 ... + e

ggplot(data = comms_summed, aes(x = AUC_z_score, y=i_plus_BX_equals_y )) +
  facet_wrap(~community, scale = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point()



#+ Saca el r2 de los AUCs medidos y los AUCs predichos de las comunidades, saca 
#+ el p y el q value. 
#+ Con esto podemos ver que hay comunidades cuya predicción se asemeja mucho a 
#+ los datos reales y es significativa, mientras que hay otras comunidades que 
#+ no pueden ser predichas solo con los datos de las cepas que las componen 
#+ 
#+ las coms predichas que se ajustan mejor a las medidas son las uqe muestran 
#+ mas dominancia de alguna cepa?

r2_signif <- comms_summed %>%
  group_by(community) %>%
  summarise(cor = cor(AUC_z_score, i_plus_BX_equals_y),
            p.value = cor.test(AUC_z_score, i_plus_BX_equals_y)$p.value,
            .groups = "drop") %>%
  mutate(q.value = p.adjust(p.value, 'fdr')) 




#+ boxplot de los coefficientes de cada bacteria para ver cuales tienen un mayor 
#+ efecto en al AUC de la comunidad

comms_info %>%
  select(community, repbio, temp, strains, coefficient) %>%
  group_by(community, temp, strains) %>%
  summarise(coefficient = unique(coefficient),
            .groups = "drop") %>%
  ggplot(aes(x = strains, y = coefficient)) +
  geom_boxplot() +
  geom_point() +
  scale_y_log10()





#+ plot que muestra las normas de reaccion medidas (experimentos) vs la norma de
#+ rxn predicha, con el R2 y el pvalue 

comms_summed <- comms_summed %>%
  mutate(community = factor(community, levels = communities))


p1 <-ggplot()+
    geom_point(data = comms_summed, aes(x=temp, y=AUC_z_score, group = repbio), colour="peachpuff4")+
    geom_line(data= comms_summed, aes(x=temp, y=AUC_z_score, group = repbio, colour = as.numeric(sub("C", "", community)), linetype = 'Measured'), linewidth=1.5)+
    
    geom_point(data = comms_summed, aes(x=temp, y=i_plus_BX_equals_y, group = repbio))+
    geom_line(data= comms_summed, aes(x=temp, y=i_plus_BX_equals_y, group = repbio, linetype = 'Prediction'), linewidth=1.5)+
    
    geom_text(data = r2_signif, aes(x = 30, y = Inf, label = paste0("R² = ", round(cor^2, 2),"\np = ", format.pval(p.value, digits = 2))),
      hjust = 'right',
      nudge_x = 12,
      vjust = 1.2,
      size = 3) +
    
    facet_wrap(~community, ncol=8)+
    scale_x_continuous(breaks=c(30,37,42))+
    scale_color_gradient(low = "blue", high = "red") +
    labs( title = "Measured and predicted community reaction norms",y=expression("AUC"), x = expression("Temperature °C"), color="Communities", linetype = "Prediction")+
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12)) 




ggsave(plot = p1,
       filename = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/ODs/prediction_rxn_norm_comsint_lm.png",
       bg="white",  width = 38, height = 20, units = "cm") 



