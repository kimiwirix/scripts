## Carpetas:
### growth_curves
  1. graphs_gc
  2. rxn_norms
  3. stat_analysis_temp
  4. tests_shakers

### metabarcoding
  1. chimeras_removal: en cluster meto un.sh que remueva las chimeras de la lista de unmatched y me da resultados. Se appendexeó en metabarcoding ensamble al final. Este script remueve la lista de unmathced seqs que son chimeras de la frequency table. 
  2. matched_unmatched: plots the matched and unmatched  
  3. v4_trimming: trimms v4 region to use in qiime 
  * batch_0:
    1. graphs_barplot_ensamble:
    2. metabarcoding_ensamble



## Files:
1. extract_filenames_headers
2. change_header_for_filename
3. pruebas_kits: se probaron tres diferentes kits para ver si todos tenían la capacidad para detectar todas las cepas. Nos quedamos con QIAGEN Powersoil
4. extintion_exp_congelacion: queríamos saber si hay una diferencia de cfus después de congelar a -80 y reactivar.
   * Concluisón: la congelación si tiene un efecto significativo sobre el survival en todas las cepas 
5. growthrate_duplicationtime_indivstrains: duplication times round about 20-30 minutes 
6. MM_coloration: there were changes in coloration in one batch made on 22.01.26, the 10 13 bottles with medium were tested on absorbance in different wavelenghts.
   * Concluisón: there is no statistical difference between the bottles based on absorbance
7. pyani:para seleccionar las cepas lo mas separadas entre ellas y capturar overall diversity
8. dendrogram: makes dendrogram out of PYANI similarity matrix between genomes
9. graphs_od: script para plottear los ods. 
10. cfus_dominance_plates_assay: se hizo protocolo geeral en una temperatura (37°C) para tres comsints (C1,C17,C31) y en vez de mandar a secuenciar en cada tiempo (0,3,6,9,12,24,36 hrs) se plaquearon 5 placas por comunidad para contar cfus. 
    * CONCLUSIÓN: el tercer pase seriado 36hrs no es tan necesario plt vamos a ver si lo quitamos y añadimos un tiempo intermedio a las 9hrs (timepoint 1.5)







