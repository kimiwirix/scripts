# intento 1 
# 9/4/2024
# manifest mas corto con menos samples (N500001A,N500002A,N500003A)
# single-end sequences obtained e 00.cleandata
# quality filter and dereplicate 
# vsearch closed reference
# se hizo single end con clean data fastq


# steps: https://forum.qiime2.org/t/closed-reference-otu-picking-steps/21952

# 1. hacer manifest 
echo -e "sample-id\tabsolute-filepath 
 N500001A\t$PWD/N500001A/N500001A.fastq
 N500002A\t$PWD/N500002A/N500002A.fastq
 "

# correr en carpeta donde están las carpetas N5000 en este caso en 00.cleandata
qiime tools import \
  --type 'SampleData[SequencesWithQuality]' \
  --input-path intento-manifest.tsv \
  --output-path intento-single-end-demux.qza \
  --input-format SingleEndFastqManifestPhred33V2

# or phred64

qiime quality-filter q-score \
    --i-demux intento-single-end-demux.qza \
    --o-filtered-sequences intento1-demux-filtered.qza \
    --o-filter-stats intento1-demux-filter-stats.qza

# se tarda añossss
qiime vsearch dereplicate-sequences \
  --i-sequences intento1-demux-filtered.qza \
  --o-dereplicated-sequences intento1-dereplicated-seqs.qza \
  --o-dereplicated-table intento1-dereplicated-table.qza

# open 
qiime vsearch cluster-features-closed-reference \
  --i-table intento1-dereplicated-table.qza \
  --i-sequences intento1-dereplicated-seqs.qza \
  --i-reference-sequences reference_seqs.qza \
  --p-perc-identity 0.85 \
  --o-clustered-table intento1-table-cr-85.qza \
  --o-clustered-sequences intento1-rep-seqs-cr-85.qza \
  --o-unmatched-sequences intento1-unmatched-cr-85.qza

qiime feature-table summarize \
  --i-table intento1-table-cr-85.qza \
  --m-sample-metadata-file metadata.tsv \
  --o-visualization intento1-wtf.qzv

qiime tools export \
--input-path intento1-table-cr-85.qza \
--output-path intento_abundance_table

biom convert -i intento_abundance_table/feature-table.biom \
-o intento_abundance_table/feature-table.tsv --to-tsv




