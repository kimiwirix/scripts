#Exp: En growth curves 26/08/25 se hicieron MPNs de todas las cepas (10) en cada temperatura (30,37,42), se congeloaron 100uL:100uL 
#     de glicerol80%:inóculo y se guardaron. El 23/09/25 se descongelaron gliceroles y se hicieron MPNs para ver si la congelación 
#     tiene un efecto sobre los CFUs/ml 

#Concluisón: la congelación si tiene un efecto significativo sobre el survival en todas las cepas 


library(readODS)
library(dplyr)
library(ggplot2)
library(pracma)
library(performance)
library(tidyr)


file_od<-read.table(file="C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/individual_strains_growth_curves_NEW_filtered.tsv", 
                    sep='\t', 
                    header = TRUE)
file_mpn<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/MPNs.ods", sheet="growth_curve")%>%
  filter(!is.na(`muestra`))%>% #filters rows I put as separation between temps in ods 
  mutate(rep=case_when(
    temp==30~1,
    temp==37~2,
    temp==42~3
  ))%>%
  select(!temp) #temperatura no importa plt se tomas las mediciones que se hicieron por temp como si fueran replicas 



f1<-file_mpn%>%
  pivot_wider(  #divide las columnas en mpn antes de la congelacion y mpn despues de la congelacion 
    names_from = repco,
    values_from = `CFUs per ml`
  ) %>% 
  mutate(survival=despues/antes)






# este modelo nos dice que tanto cambian las cepas despues comparando con el antes de la cepa ch111, plt ese es el efecto de la congelacion 
# esto esta calculado con base en survival que es cfus despues/antes. al calcular el survival da igual con cuanta cantidad se empieza o se termina al final es un ratio si el ratio 
# se acerca a 0 es porque se murieron muchas despues de la congelacion y si se acerca a 1 es que la congelacion no mato a niguna 


m1 <- glm(despues ~ log(antes) + muestra, 
          data = f1 %>% mutate(rep = factor(rep)),
          family = poisson(link = "log"))
summary(m1)


a1<-aov(survival~muestra, data=f1)
summary(a1)



#Is the average survival ratio smaller than 0.5? 
#the average survival ratio is 0.32
t.test(f1$survival, mu = 0.5, alternative = "less")




#plots
ggplot(data=f1, aes(x=muestra, y=survival, colour=as.factor(rep)))+
  geom_point()

