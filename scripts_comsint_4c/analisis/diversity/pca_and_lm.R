#+ PCA failed to model the data so sur did graph to protray differences in shannon indexes 
#+ done by community and temperature 


library(tidyverse)
library(readODS)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggfortify)
library(vegan)
library(ape)
library(ggnewscale)
library(tidyr)
library(reshape2)


strain_ids<-c("CH111","CH90","CH161d", "CH149a","CH29","CH99b","CH154a","CH23","CH447", "CH450")  


#cambiar dependiendo el archivo 
f <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/matched.tsv", sep = "\t",
              header = TRUE) 

m <- f %>%
  filter(X.OTU.ID %in% strain_ids) %>%
  column_to_rownames(var = "X.OTU.ID")


#+ makes proportion table and transposes it and adds an extra column of the 
#+ labels of the community in format CC0000X

proportion_table<-apply(m, 2, function(x) x/sum(x)) 
p<-proportion_table %>%   
  t() %>% 
  as.data.frame()
p$label_final<-rownames(p)




#+ METADATA is already a nonchanging db

m <- read.table("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/metadata_db.tsv")



#+ Proportion table and metadata together based on the label name

pm <- merge(p, m, by = "label_final")    
pm [, 2:11] <- apply(pm[, 2:11], 2, as.numeric)




#+ We separate the frequencies and the metadata que componen el df pm. 
#+ In the summary we can see that the 78% of the overall variance is explained by 
#+ the 1st and 2nd PC. 

freqs <- pm %>%
  select(all_of(strain_ids))

meta<- pm %>%
  select(!all_of(strain_ids)) %>%
  mutate(hrs = as.numeric(hrs))

pca <- prcomp(freqs, center = TRUE)
summary(pca)



#+ Then we want to see how the data is acomodada by plotting the two PCs that 
#+ account for the most variation, however if we colour the plot by the variables
#+ temp, batch, timepoint of repbio it does not show a clear pattern. So what 
#+ is giving the plot this triangular shape? 

comp <- pca$x %>%
  bind_cols(meta)

ggplot(data = comp, aes(x=PC1, y=PC2))+
  geom_point()



#+ The figure is a trriangle-like and with loadings we get to see that some strains
#+ are the ones that account for that shape 
#+ so the variation in the PC1 and PC2 are mainly driven by three dominant species:
#+ CH23: Bac. altitudinis
#+ CH90: Bac. artophaeus
#+ CH111: Bac. thuringiensis

loadings <- pca$rotation[, 1:2]
loadings

ggplot(as.data.frame(loadings),
  aes(x = PC1, y = PC2, label = rownames(loadings))) +
  geom_point() +
  geom_text(vjust = -0.5) 


#+ So now we have to do three subpcas one for each strain and see what variable 
#+ explains the variance 

bac_alt <- c("C1","C2","C3","C4","C5","C7","C8","C9","C11","C12","C14","C17","C18","C20","C23","C27")
bac_thur<- c("C1","C2","C4","C5","C6","C8","C9","C10","C14","C15","C16","C20","C21","C22","C26","C30")
bac_art <- c("C6","C10","C13","C15","C16","C19","C21","C22","C24","C25","C26","C28","C29","C30","C31","C32")



freqs_alt <- freqs %>%
  bind_cols(meta) %>%
  filter(community %in% bac_alt)%>%
  select(all_of(strain_ids))

freqs_art <- freqs %>%
  bind_cols(meta) %>%
  filter(community %in% bac_art)%>%
  select(all_of(strain_ids))

freqs_thur <- freqs %>%
  bind_cols(meta) %>%
  filter(community %in% bac_thur)%>%
  select(all_of(strain_ids))


pca_alt <- prcomp(freqs_alt, center = TRUE)
summary(pca_alt)

pca_art <- prcomp(freqs_art, center = TRUE)
summary(pca_art)

pca_thur <- prcomp(freqs_thur, center = TRUE)
summary(pca_thur)


p_art <- pca_art$x %>%
  bind_cols(meta%>%
              filter(community %in% bac_art))


#+ The pcas are fine but when we plot them they still have the triangular shape, so 
#+ what now? we can do loadings again to see if strains are again explaining the shape 
#+ and variance of each sub pca

ggplot(data = p_art, aes(x=PC1, y=PC2))+
  geom_point(aes(color=as.factor(techrep)), size=2)
  

loadings <- pca_art$rotation[, 1:2]
loadings

ggplot(as.data.frame(loadings),
       aes(x = PC1, y = PC2, label = rownames(loadings))) +
  geom_point() +
  geom_text(vjust = -0.5) 


#+ Apparently even though we already separated the main pca into dominant strains
#+ there are again dominant strains, so we can conclude that strains present
#+ in each community account for the most variation however, due to the shape and 
#+ experimental design the PCA is not useful for this analysis:(







####sur

#+ Añade una columna con índice shannon de alpha diversidad 

pm$shannon <- vegan::diversity(pm[,2:11], "shannon")

#+ orders comsints from biggest mean shannon index for each community (the mean
#+ of the three temps) to the lowest shannon index 
#+ 
#+ Alpha diversity in microbiology: 
#+ the observed richness (number of taxa) or evenness (the relative abundances of those taxa) 
#+ of an average sample within a habitat type.

syncoms_ordered <- pm %>% 
  group_by(community) %>% 
  summarise(mean_shannon = mean(shannon), .groups = "drop") %>% 
  arrange(desc(mean_shannon)) %>% 
  select(community) %>% 
  unlist

s <- pm %>% 
  group_by(community) %>% 
  summarise(mean_s=mean(shannon))  %>% 
  mutate(community = factor(community, levels = syncoms_ordered))


#+ makes grpah that plots the communities in x axis ordered by larger shannon index
#+ to lowest 

pt <-ggplot(pm %>% 
             mutate(community = factor(community, levels = syncoms_ordered)), 
           aes(x = community, y = shannon)) + 
  facet_wrap(~community, scales = "free_x", nrow = 1) + 
  geom_boxplot(aes(fill = factor(temp)), position = position_dodge(width = 1))  + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("30"="#63B8FF", "37"="lightsalmon", "42"="indianred3")) +
  labs(y = "Shannon index", x = "Community", fill="Temperature (°C)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(
    data = s,
    aes(yintercept = mean_s),
    color = "red",
    linewidth = 0.8,
    inherit.aes = FALSE
  )



pt
ggsave(plot = pt,
       filename = "C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/shannon_boxplots_w_mean.png",
       bg="white",  width = 50, height = 15, units = "cm")

#+ explicacion de grafica: podemos ver que en cuanto a alpha diversidad, el efecto 
#+ de la comunidad, es decir de que cepas hay en que comunidad(indicar en eje y)
#+ es mayor al efecto de la temperatura indicar la comunidad con mayor efecto en
#+ temp. Y ademas la temperatura tiene un efecto muy varible en las comunidades 
#+ nunca es el mismo. 
#+ 
#+ El rango de índices de diversidad entre las comunidades es mayor al rango de
#+ diversidad por efecto de temperatura. 




#+ Esto se puede comprobar con un anova en donde la proporcion del efecto de la 
#+ comunidad es del 30% aprox sobre la variacion total mientras que el efecto 
#+ de la temperatura es del 18%

m<-lm(shannon ~ community *  temp + hrs + repbio, data = pm)
summary(m)
anova(m)


library(performance)
check_model(m)

#+ The interaction means: 
#+ Does the effect of temperature on Shannon diversity depend on which community it is?
#+ example: temp can increase diversity in one comsint and decrease in other 
#+ If we put + instead of * we would be assuming that temp effect is the same for all comsints









