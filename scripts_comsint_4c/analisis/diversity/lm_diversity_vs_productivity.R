#+ Relation between alpha-diversity (shannon) and productivity (qPCR and OD)

library(performance)
library(ggplot2)
library(vegan)
library(lme4)
library(dplyr)
library(textshape)



#+ Load matched table to calculate the alpha diversity over the proportion 
#+ table (p). Load the metadata info to get the batches since those are not 
#+ 

matched <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/matched.tsv", sep = "\t",
              header = TRUE) %>%
  column_to_rownames(loc = "X.OTU.ID") 
  

proportion_table <- apply(matched, 2, function(x) x/sum(x)) 
p<-proportion_table %>%   
  t() %>% 
  as.data.frame()
p$label_final<-rownames(p)
p$shannon <- diversity(p[,1:10], "shannon")



#+ qPCR data that is joined to the proportion table p in an all dataframe (a)
comsints_ordered <- paste0("C", 1:32)


qpcr <- read.csv("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/qPCRs_db.tsv", sep = "\t",
                 header = TRUE) 
a <- p %>%
  right_join(qpcr, by = 'label_final') %>%
  mutate(community = factor(community, levels = comsints_ordered)) %>%
  filter(!timepoint==0) %>%
  mutate(batch=paste("batch", batch, sep = ''))
  





#+ lm is simpler model 
#+ lmer is more complex model: the productivity is shaped by the diversity, the 
#+ temperature and the diversity within each community and the effect of batch. 
#+ The first two factors (shannon+temp) are calculated on the overall data, 
#+ while the third factor (shannon|comm) takes into account the effect of the 
#+ community on the diversity and how that affects the productivity (m_Fragmentos)
#+ 
#+ get pvalues of lmer model with lmerTest

m1 <- lm(m_Fragmentos_16S_ml ~ shannon, data = a )
summary (m1)

m2<-lm(log(m_Fragmentos_16S_ml) ~ shannon* community + temp  + repbio, data = a)
summary(m2)
anova(m2)

m1 <- lmer(log(m_Fragmentos_16S_ml) ~ shannon + temp + (shannon|community) + (1|batch) , data = a)
summary(m1)


#+ CONCLUSION: los fixed effects (shannon+temp) tienen poco efecto sobre la 
#+ overall productivity. Mientras que la mayor parte (75%) de la variación 
#+ observada en los datos de prod se debe al efecto de la diversidad presente 
#+ en cada comunidad



#+ dotplot me dice que el efecto que tiene shannon sobre la produccion de 
#+ biomasa es diferente para cada comunidad, es decir, no tiene un efecto global.
#+ Si linea está antes de 0 es desviacion negativa de la media, y si está 
#+ despues es positiva

dotpt <- lattice::dotplot(ranef(m1, condVar = TRUE))$community
png("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/diversity/diver_vs_prod_lattice.png")
print(dotpt)

dev.off()



p1 <- ggplot(data = a, aes(x = shannon, y = log10(m_Fragmentos_16S_ml),  color = as.factor(temp))) +
  geom_point() +
  geom_smooth(aes(group = community), se = FALSE, method = "lm", color='black') +
  facet_wrap(~community, ncol=8) +
  scale_color_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3")) +
  labs(title='Effect of diversity on productivity', y = "log10(mean 16S/ml fragments)", x = "Shannon index", color="Temperature (°C)") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom")




ggplot(a, aes(x = community, y = log10(m_Fragmentos_16S_ml))) + 
  facet_wrap(~community, scales = "free_x", nrow = 1) + 
  geom_boxplot(aes(fill = factor(temp)), position = position_dodge(width = 1))  + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3")) +
  theme(plot.title = element_text(hjust = 0.5)) 



ggsave(plot = p1,
       filename = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/diversity/diversity_vs_productivity.png",
       bg="white",  width = 30, height = 17, units = "cm")
ggsave(plot = dotpt,
       filename = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/diversity/diver_vs_prod_lattice.png",
       bg="white",  width = 30, height = 17, units = "cm")








#+ OD data yields same results as qPCR in terms of neggative deviation from the 
#+ avg response but no positive deviation 

od <- read.csv("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/ODs_db.tsv", sep = "\t",
                 header = TRUE) 

a2<- p %>%
    inner_join(od, by = 'label_final')

m1 <- lmer(OD_real ~ shannon + temp + (shannon|community)+(1|batch), data = a2)
summary(m1)

dotpt <- lattice::dotplot(ranef(m1, condVar = TRUE))$community

