#+ Script que me enseñe la grafica de una rxn norm de una comunidad y al 
#+ lado las rxn norms de las cepas individuales que la componen esa comsint 

library(patchwork)
library(ggplot2)
library(dplyr)

#+ Files para hacer las gráficas de rxn norms de las comsints: 
#+ b_AUC: tiene la info del AUC de las tres réplicas de cada comsint en cada temp
#+ b_mean: tiene la info del mean de las tres réplicas para hacer la línea punteada

b_AUC <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/ODs/AUC_comsints.tsv", header = TRUE,  sep='\t' )
b_mean <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/ODs/AUC_mean_comsints.tsv", header = TRUE,  sep='\t' )


#+ Files para hacer las gráficas de rxn norms de las cepas individuales:
#+ t_AUC: info de AUC de tres réplicas por condición de temp, las réplicas son variadas
#+ porque se tuvieron que repetir gc debido a problemas con shakers de incubadoras 
#+ por eso no se puede dibujar una línea por rxnnorm como en el caso de las comsints
#+ pero para las cepas indiv las réplicas se parecen mucho entre si 
#+ t_mean: como no se puede dibujar una linea por réplica se dibuja una línea de la 
#+ mean de los puntos  

t_AUC <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/gc_and_rxnnorm/AUC_indiv_strains.tsv", header = TRUE,  sep='\t' )
t_mean <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/gc_and_rxnnorm/AUC_mean_indiv_strains.tsv", header = TRUE,  sep='\t' )



#+ What strains are in which comsints 

presence <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/CC_dbs/strains_in_comsints.tsv", header = TRUE,  sep='\t' ) %>%
  filter(presence==1)



#+ Hace las gráficas de las rxn norms de las comsints y se va una por una con el
#+ for loop, y al final en assign, guarda el plot con el nombre de la comsint

communities<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32")

for (comsint in communities) {
  p <- ggplot()+
    geom_point(data = b_AUC%>%
                 filter(community == comsint), aes(x=temp, y=AUC, group = repbio), colour="peachpuff4") +
      geom_line(data= b_AUC%>%
                  filter(community == comsint), aes(x=temp, y=AUC, group = repbio, colour = community_num), linewidth=1.5) +
      geom_line(data=b_mean %>%
                  filter(community == comsint), aes(x=temp,y=mean_AUC),linetype='dotted', linewidth=0.8)+
      scale_x_continuous(breaks = c(30,37,42))+
      scale_y_continuous(limits= c(7,18.6), breaks = seq(7,18.6, by= 2.5))+
      labs( title = paste(comsint, "reaction norm"), y=expression("AUC"), x = expression("Temperature °C"))+
      theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 12),
            legend.position = "none") 
    
  assign(comsint, p)
    
}




#+ Hace las gráficas de las rxn norms de las indiv strains y se va una por una 
#+ con el for loop, y al final en assign, guarda el plot con el nombre de la 
#+ strain

strains<-c("Bacillus altitudinis", "Corynebacterium sp.", "Staphylococcus arlettae", "Bacillus thuringiensis", "Staphylococcus shinii", "Bacillus atrophaeus",  "Micrococcus luteus",  "Bacillus infantis", "Priestia megaterium", "Metabacillus indicus")
custom_colors <- c("Bacillus altitudinis"="#273a3eff", "Corynebacterium sp."="#08519cff",  "Staphylococcus arlettae"="#00e5eeff", "Bacillus thuringiensis"="#4682b4ff",  "Staphylococcus shinii"="#bdd7e7ff", "Bacillus atrophaeus"="#8c2424ff", "Micrococcus luteus"="#cd2626ff", "Bacillus infantis"="#ff0000ff", "Priestia megaterium"="#ef6d53ff", "Metabacillus indicus"="#fcae91ff" )
italic <- setNames(paste0("italic(\"", strains, "\")"), strains)


for (strain in strains) {
  q <- ggplot()+
    geom_point(data=t_AUC%>%
                 filter(Cepa == strain),aes( x = temp, y = AUC, group = rep), colour="peachpuff4")+
    geom_line(data = t_mean%>%
                filter(Cepa == strain), aes( x = temp, y = mean_AUC, group = Cepa, color=Cepa), 
              linewidth = 1.5)+
    scale_color_manual(values = custom_colors)+
    facet_wrap(~Cepa, ncol = 5, labeller = as_labeller(italic, label_parsed))+
    scale_x_continuous(breaks = c(30,37,42))+
    scale_y_continuous(limits= c(0,15), breaks = seq(0,15, by= 5))+
    labs( x = expression("Temperature °C"))+
    theme(legend.position = "none")
  
  assign(strain, q)
}



#+ hace una grafica para cada comsint en donde esta la grafica de la comsint y
#+ al lado están las 5 graficas de las cepas que componen esa comsint. De 
#+ todas son las rxn norms. El output es una grafica por comsint. 

combined_plots <- list()


for (comsint in unique(presence$community)) {
  
  # Get the 5 strains belonging to this community
  strains <- presence %>%
    filter(community == comsint, presence == 1) %>%
    pull(real_name)
  
  # Get the community plot
  community_plot <- get(comsint)
  
  # Get the 5 strain plots
  strain_plots <- lapply(strains, function(s) {
    get(s) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  # Combine: community + 5 strains in ONE ROW
  combined_plots[[comsint]] <- wrap_plots(
    c(list(community_plot), strain_plots),
    nrow = 1,
    ncol = 6)
  
  
  ggsave(plot = combined_plots[[comsint]],
         filename = file.path(paste0("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/ODs/plots_comsint_vs_indiv/", comsint, ".png")),
         bg="white",  width = 30, height = 7, units = "cm")   
  

}





