
# 10/4/2024
# 16S raw data analysis 
# raw data /mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/01.RawData
# manifest all 60 samples NS00001A-NS00006A,NS00013A-NS00036A,NS00049A-NS00078A
# paired-end sequences
# pipeline: import, vsearch joinpairs, quality filter, vsearch dereplicate, vsearch closed-reference, export
# steps: https://forum.qiime2.org/t/closed-reference-otu-picking-steps/21952

#Pasos
#1. Referencias: Meter abi files en programa R para convertir a fasta (_contig.fasta es el que nos sirve)
#2. Referencias: Quitar espacios \n de cada fasta en Notepad++
#3. Referencias: Unir todos los fasta en 1 documento en powershell: cat *_contig.fa > reference_seqs_sangercontigR


#4. Referencias: importar fasta file con referencias 
qiime tools import \
  --type 'FeatureData[Sequence]' \
  --input-path reference_seqs_sangercontigR.txt \
  --output-path reference_seqs.qza

# 5. Muestras: hacer manifest  
echo -e "sample-id\tforward-absolute-filepath\treverse-absolute-filepath
N500001A\t/mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/01.RawData/N500001A/N500001A.raw_1.fastq\t/mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/01.RawData/N500001A/N500001A.raw_2.fastq"
#name:manifest.tsv in /mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/intento2


# 6. Muestras: importea sequencias a artifacto .qza
#quitar el .gz de archivos y de manifest (gunzip -r NS*)
qiime tools import \
  --type 'SampleData[PairedEndSequencesWithQuality]' \
  --input-path manifest.tsv \
  --output-path paired-end-demux.qza \
  --input-format PairedEndFastqManifestPhred33V2

# 7. Muestras: une secuencias R y F
#in latest versions join-pairs changed to merge-pairs
qiime vsearch join-pairs \
 --i-demultiplexed-seqs paired-end-demux.qza \
 --o-joined-sequences paired-end-merged.qza 

# Paso extra: para ver la calidad de las secuencias?
qiime demux summarize \
  --i-data paired-end-merged.qza \
  --o-visualization merged-sequences.qzv

# 8. Filtro por q score, trimmea los ends que esten muy degradados 
qiime quality-filter q-score \
    --i-demux paired-end-merged.qza \
    --o-filtered-sequences demux-filtered.qza \
    --o-filter-stats demux-filter-stats.qza

# 9. Agrupa las secuencias repetidas para que la comparcion con las referencias no sea tan larga
# se tarda añossss
qiime vsearch dereplicate-sequences \
  --i-sequences demux-filtered.qza \
  --o-dereplicated-sequences dereplicated-seqs.qza \
  --o-dereplicated-table dereplicated-table.qza

# comparacion con secuenacias
# 10. closed 
qiime vsearch cluster-features-closed-reference \
  --i-table dereplicated-table.qza \
  --i-sequences dereplicated-seqs.qza \
  --i-reference-sequences reference_seqs.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-cr-85.qza \
  --o-clustered-sequences rep-seqs-cr-85.qza \
  --o-unmatched-sequences unmatched-cr-85.qza

 
# 10. open 
qiime vsearch cluster-features-open-reference \
  --i-table dereplicated-table.qza \
  --i-sequences dereplicated-seqs.qza \
  --i-reference-sequences reference_seqs.qza \
  --p-perc-identity 0.97 \
  --o-clustered-table table-cr-85.qza \
  --o-clustered-sequences rep-seqs-cr-85.qza \
  --o-new-reference-sequences new-references-cr-85.qza

# 11. exportea la tabla de frecuencias 
qiime tools export \
--input-path table-cr-85.qza \
--output-path abundance_table

# 12. convierte tabla a tsv
biom convert -i abundance_table/feature-table.biom \
-o abundance_table/feature-table.tsv --to-tsv


# if summary featrue table vizualization needed (para comprobar parametros)
qiime feature-table summarize \
  --i-table table-cr-85.qza \
  --m-sample-metadata-file metadata.tsv \
  --o-visualization feature-table-viz.qzv

#convertirunmatched sequences a directorio con fasta 
qiime tools export \
    --input-path new-references-cr-85.qza \
    --output-path unmatched-sequences






###NOTA IMPORTANTE: 
###cuando se hace en qiime el análisis, hay que quitar los primers de las secuencias porque se usan primers degenerados
###y eso tambien los cuenta qiime al momento de clusterizar en otus




###YA NO 
##REFERENCES
#convertirr sequencias de referencias de abi to fastq
  seqret -sformat abi -osformat fastq -auto -stdout -sequence NS_042_27F_Premix.ab1 > NS_042_27F_Premix.fastq


#reverse complement de 1492R 
  fastx_reverse_complement -i NS_042_1492R_Premix.fastq -o NS_042_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_046_1492R_Premix.fastq -o NS_046_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_060_1492R_Premix.fastq -o NS_060_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_094_1492R_Premix.fastq -o NS_094_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_101_1492R_Premix.fastq -o NS_101_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_109_1492R_Premix.fastq -o NS_109_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_110_1492R_Premix.fastq -o NS_110_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_143_1492R_Premix.fastq -o NS_143_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_154_1492R_Premix.fastq -o NS_154_1492R_Premix_revcom.fastq
  fastx_reverse_complement -i NS_164_1492R_Premix.fastq -o NS_164_1492R_Premix_revcom.fastq






#falta
#hacer arbol de sequencias referencias 
#notas grales: hacer reference fasta file con id bien 



qiime tools export \
  --input-path new-references-cr-85.qza \
  --output-path newreference.fasta



