+En contamination_matched.R están los cambios a la matched abundance table, quitando muestras muy contaminadas (>20% de contaminacion) y cambiando a 0 los reads que son ruido (reads que son contaminación y están debajo de 50). Tambien está la tabla de metadata sin las muestras muy contaminadas (>20% de contaminacion) y los labels que están cambiados de A y B y el lost sample. 

+comsint_singlebact_comparison_graph.R for the comparison between OD of comsints and theoretical OD of single strains experiments. 

+absolute_frequencies_mpn: barplots pero con frequencias absolutas y no relativas solo del día en el que se hicieron los MPNs 

+graphs_1.R plotea the effect of temperature in the 10 strains across comsints, in boxplots 

+graphs_2.R grafica barplots con tablas de frequecias ya sin contaminación

+contaminacion_matched: produces f_clean and metadata_clean, both without contaminated samples (cont>20%) and without noise abundances (<50)

linear_models<- output. a heatmap of the signifficant effects of the factors temp and proportion of strains in each comsint 