# intento 2
# 9/4/2024
# manifest mas corto con menos samples (N500001A,N500002A,N500003A)
# paired-end sequences obtained de ##############
# quality filter and dereplicate 
# vsearch closed reference
# se hizo single end con clean data fastq
# steps: https://forum.qiime2.org/t/closed-reference-otu-picking-steps/21952


#notas grales: hacer reference fasta file con id bien 


# poner todas las references en un fasta file e importearlas
qiime tools import \
  --type 'FeatureData[Sequence]' \
  --input-path reference_seqs.fasta \
  --output-path reference_seqs.qza


# 1. hacer manifest  
echo -e "sample-id\tforward-absolute-filepath\treverse-absolute-filepath
N500001A\t/mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/01.RawData/N500001A/N500001A.raw_1.fastq\t/mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/01.RawData/N500001A/N500001A.raw_2.fastq"
#name:manifest.tsv in /mnt/atgc-d3/sur/users/nsaid/2024-06.novogene_16S_NS1_NS2/Batch_1/result_X202SC23054171-Z01-F001/intento2


# correr en carpeta donde están las carpetas N5000 en este caso en 00.cleandata
##quitar el .gz de archivos y de manifest
qiime tools import \
  --type 'SampleData[PairedEndSequencesWithQuality]' \
  --input-path manifest.tsv \
  --output-path paired-end-demux.qza \
  --input-format PairedEndFastqManifestPhred33V2


#in latest versions join-pairs changed to merge-pairs
qiime vsearch join-pairs \
 --i-demultiplexed-seqs paired-end-demux.qza \
 --o-joined-sequences paired-end-merged.qza 


qiime demux summarize \
  --i-data paired-end-merged.qza \
  --o-visualization merged-sequences.qzv


qiime quality-filter q-score \
    --i-demux paired-end-merged.qza \
    --o-filtered-sequences demux-filtered.qza \
    --o-filter-stats demux-filter-stats.qza


# se tarda añossss
qiime vsearch dereplicate-sequences \
  --i-sequences demux-filtered.qza \
  --o-dereplicated-sequences dereplicated-seqs.qza \
  --o-dereplicated-table dereplicated-table.qza


# open 
qiime vsearch cluster-features-closed-reference \
  --i-table dereplicated-table.qza \
  --i-sequences dereplicated-seqs.qza \
  --i-reference-sequences reference_seqs.qza \
  --p-perc-identity 0.85 \
  --o-clustered-table table-cr-85.qza \
  --o-clustered-sequences rep-seqs-cr-85.qza \
  --o-unmatched-sequences unmatched-cr-85.qza


qiime tools export \
--input-path table-cr-85.qza \
--output-path abundance_table

biom convert -i abundance_table/feature-table.biom \
-o abundance_table/feature-table.tsv --to-tsv




# if summary featrue table vizualization needed (para comprobar parametros)
qiime feature-table summarize \
  --i-table table-cr-85.qza \
  --m-sample-metadata-file metadata.tsv \
  --o-visualization feature-table-viz.qzv

 