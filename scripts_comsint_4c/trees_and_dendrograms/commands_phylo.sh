#!/bin/bash

#SBATCH --job-name=4c_phylo
#SBATCH --output=%x.log
#SBATCH --error=%x.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8       # 8 CPUs, as requested
#SBATCH --time=1:00:00         # hh:mm:ss  (identify is the slow step; ~<1 h realistically)
#SBATCH --mem=20G               # see note below: NO pplacer here, so far less than the 120 G classify job

# =====================================================================
#  RP phylogenomic tree — bac120 concatenated-marker alignment
#  Genomes: 3 dereplicated isolates (RP7, RPM, RPR) + 18 references = 21
#  This job runs ONLY GTDB-Tk identify + align (needs the GTDB reference
#  data + RAM). Tree inference (IQ-TREE / FastTree) is cheap and can run
#  here at the end OR off-cluster — see the two OPTION blocks below.
# =====================================================================

echo "===== Starting job ($SLURM_JOB_ID) ====="
date

source $GROUP_HOME/modules/pkgs/mamba/main/etc/profile.d/conda.sh
conda activate gtdbtk-2.6.1     # same env & DB (r226) used for the original classification

# GTDBTK_DATA_PATH must point at the r226 reference package (already set in your
# gtdbtk-2.6.1 env, since classify_wf worked). Sanity check:
# gtdbtk check_install || { echo "GTDB-Tk reference data not found"; exit 1; }

# ---------------------------------------------------------------------
# 1) Identify bac120 marker genes in all 21 genomes
#    genomes/ must contain the *.fna files
# ---------------------------------------------------------------------
gtdbtk identify \
   --genome_dir fnas/ \
   --out_dir gtdbtk \
   --extension fna \
   --cpus 8

# ---------------------------------------------------------------------
# 2) Align -> concatenated bac120 MSA (user genomes only)
#    Produc|es: gtdbtk_out/align/gtdbtk.bac120.user_msa.fasta.gz
# ---------------------------------------------------------------------
gtdbtk align \
   --identify_dir gtdbtk \
   --out_dir gtdbtk \
   --cpus 8

# unzip the user MSA for the tree step
gunzip -k gtdbtk/align/gtdbtk.bac120.user_msa.fasta.gz
MSA=gtdbtk/align/gtdbtk.bac120.user_msa.fasta
echo "MSA taxa / columns:"
grep -c '^>' "$MSA"
head -2 "$MSA" | tail -1 | tr -d '\n' | wc -c

# ---- IQ-TREE ----------
# Requires iqtree2
# ModelFinder + 1000 ultrafast bootstraps + SH-aLRT, rooted on Thermotoga

module load iq-tree/2.4.0
mkdir 4c_bact120_iqtree

if command -v iqtree2 >/dev/null 2>&1; then
    iqtree2 -s "$MSA" \
        -m MFP \
        -B 1000 -alrt 1000 \
        -o Micrococcus_luteus_149_a,Corynebacterium_sp_29_x \
        -T AUTO --threads-max 8 \
        --prefix 4c_bact120_iqtree/gtdbtk # Change to desired output
fi

echo "===== Ending job ($SLURM_JOB_ID) ====="
date

echo "===== sacct ====="
sleep 5
sacct -j $SLURM_JOB_ID --format=JobID,JobName,Elapsed,MaxRSS,MaxVMSize,TotalCPU,ReqMem,State,ExitCode
