# MPN strain: ST00085 // 2023/08/10 
rm(list=ls())
install.packages("MPN")
library(MPN)
tubes=  c(6,6,6,6,6,6,6,6,6) #número de réplicas por dilución 
positives = c(6,6,6,6,6,6,2,0,0) #positivos en cada columna 
#40 miul entre diluciones, la prumera columna de 1x10-1 sus réplicas se le suma 1x10-1, o sea 1x10-2 y para convertir los miul a ml son 1x10-3 entonces se suman
#esos dos y da 1x10-4 para la primer a columna de réplicas 
amount = 40 * c(10**-4,10**-5,10**-6,10**-7,10**-8,10**-9,10**-10,10**-11,10**-12)#, 10**-13) #
mpn <- mpn(positives, tubes, amount)
format(mpn[2], scientific = TRUE)

#Usar para cuanod se hizo dilucion 100:100 glicerol:muestra 
#format(as.numeric(mpn[2])*2, scientific = TRUE)

#de los resultados, usar el MPN_adj





