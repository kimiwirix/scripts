#+ HECHO POR CHATGPT
#+ 
#+ script que examina todos los alineamientos creados por muscle. 
#+ Quiero encontrar genes que sean ortólogos y de una sola copia entre las diez 
#+ cepas y que tengan regiones conservadas ente ellos para hacer primers y poder
#+ ajustar los reads de 16S comparando los reads de un gen monocopia, porque no
#+ sabemos cuántas copias del 16S tiene cada cepa. CHECAR EXPLICACIONES. 
#+ 
#+ En el cluster tengo 327 fasta files. Cada fastafile tiene un gen que monocopia 
#+ que comparten las 10 cepas. Con MUSCLE se hicieron los alineamientos de cada fasta
#+ y ahora tengo 327 .afa files (alineamientos). 
#+ 
#+ Necesito encontrar que genes(fasta) tienen el mayor grado de alineamiento 
#+ y fragmentos mas conservados para poder diseñar primers.

# ============================================================
# STEP 6: Rank orthogroups by nucleotide conservation
# ============================================================

library(Biostrings)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# ------------------------------------------------------------
# 1. INPUT / OUTPUT
# ------------------------------------------------------------

alignment_dir <- "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Orthogroup_FASTAs/alignments_muscle/"

output_dir <- alignment_dir

# ------------------------------------------------------------
# 2. PARAMETERS
# ------------------------------------------------------------

# Primer-sized windows
min_window <- 18
max_window <- 25

# Minimum conservation required
# Start with 0.90, then you can make it stricter (e.g. 1.00)
min_conservation <- 0.90

# Require every sequence to have a nucleotide
# (no gaps in the candidate primer region)
require_gap_free <- TRUE


# ============================================================
# FUNCTION 1: Calculate overall pairwise nucleotide identity
# ============================================================

calculate_pairwise_identity <- function(mat) {
  
  nseq <- nrow(mat)
  
  pairwise_identities <- c()
  
  # Compare every pair of sequences
  for (i in 1:(nseq - 1)) {
    
    for (j in (i + 1):nseq) {
      
      seq1 <- mat[i, ]
      seq2 <- mat[j, ]
      
      # Only positions where BOTH sequences have nucleotides
      valid <- seq1 != "-" & seq2 != "-"
      
      seq1_valid <- seq1[valid]
      seq2_valid <- seq2[valid]
      
      if (length(seq1_valid) > 0) {
        
        identity <- sum(seq1_valid == seq2_valid) /
          length(seq1_valid)
        
        pairwise_identities <- c(
          pairwise_identities,
          identity
        )
      }
    }
  }
  
  mean(pairwise_identities)
}


# ============================================================
# FUNCTION 2: Analyze one alignment
# ============================================================

analyze_alignment <- function(file,
                              min_window = 18,
                              max_window = 25,
                              min_conservation = 0.90) {
  
  aln <- readDNAStringSet(file)
  
  seqs <- as.character(aln)
  
  mat <- do.call(
    rbind,
    strsplit(seqs, split = "")
  )
  
  nseq <- nrow(mat)
  npos <- ncol(mat)
  
  
  # ----------------------------------------------------------
  # Overall pairwise identity
  # ----------------------------------------------------------
  
  mean_pairwise_identity <- calculate_pairwise_identity(mat)
  
  
  # ----------------------------------------------------------
  # Position-by-position conservation
  # ----------------------------------------------------------
  
  conservation <- numeric(npos)
  gap_fraction <- numeric(npos)
  
  for (i in seq_len(npos)) {
    
    column <- mat[, i]
    
    gap_fraction[i] <- mean(column == "-")
    
    nucleotides <- column[column != "-"]
    
    if (length(nucleotides) == 0) {
      
      conservation[i] <- 0
      
    } else {
      
      counts <- table(nucleotides)
      
      conservation[i] <- max(counts) / length(nucleotides)
    }
  }
  
  
  # ----------------------------------------------------------
  # General statistics
  # ----------------------------------------------------------
  
  mean_position_conservation <- mean(conservation)
  
  percent_100_conserved <-
    mean(conservation == 1) * 100
  
  percent_90_conserved <-
    mean(conservation >= 0.90) * 100
  
  percent_gap_free <-
    mean(gap_fraction == 0) * 100
  
  
  # ----------------------------------------------------------
  # Find conserved primer-sized windows
  # ----------------------------------------------------------
  
  candidate_regions <- list()
  
  candidate_counter <- 1
  
  for (window_size in min_window:max_window) {
    
    if (window_size > npos) {
      next
    }
    
    for (start in seq_len(npos - window_size + 1)) {
      
      end <- start + window_size - 1
      
      window_mat <-
        mat[, start:end, drop = FALSE]
      
      
      # Skip windows containing gaps
      if (any(window_mat == "-")) {
        next
      }
      
      
      # Conservation of this window
      window_conservation <-
        conservation[start:end]
      
      mean_window_conservation <-
        mean(window_conservation)
      
      min_window_conservation <-
        min(window_conservation)
      
      
      # Skip poorly conserved windows
      if (mean_window_conservation < min_conservation) {
        next
      }
      
      
      # Consensus sequence
      consensus <- apply(
        window_mat,
        2,
        function(x) {
          
          tab <- table(x)
          
          names(tab)[which.max(tab)]
        }
      )
      
      consensus <-
        paste0(consensus, collapse = "")
      
      
      # Save candidate
      candidate_regions[[candidate_counter]] <-
        data.frame(
          orthogroup =
            tools::file_path_sans_ext(
              basename(file)
            ),
          
          start = start,
          
          end = end,
          
          length = window_size,
          
          mean_conservation =
            mean_window_conservation,
          
          min_conservation =
            min_window_conservation,
          
          sequence = consensus,
          
          stringsAsFactors = FALSE
        )
      
      
      candidate_counter <-
        candidate_counter + 1
      
    }   # END start loop
    
  }     # END window-size loop
  
  
  # ----------------------------------------------------------
  # Combine candidates
  # ----------------------------------------------------------
  
  if (length(candidate_regions) > 0) {
    
    candidate_regions <-
      bind_rows(candidate_regions)
    
  } else {
    
    candidate_regions <-
      data.frame(
        orthogroup = character(),
        start = integer(),
        end = integer(),
        length = integer(),
        mean_conservation = numeric(),
        min_conservation = numeric(),
        sequence = character()
      )
  }
  
  
  # ----------------------------------------------------------
  # Return results
  # ----------------------------------------------------------
  
  list(
    
    summary = data.frame(
      
      orthogroup =
        tools::file_path_sans_ext(
          basename(file)
        ),
      
      n_sequences = nseq,
      
      alignment_length = npos,
      
      mean_pairwise_identity =
        mean_pairwise_identity,
      
      mean_position_conservation =
        mean_position_conservation,
      
      percent_100_conserved =
        percent_100_conserved,
      
      percent_90_conserved =
        percent_90_conserved,
      
      percent_gap_free =
        percent_gap_free
    ),
    
    candidates = candidate_regions
  )
  
}   # END analyze_alignment()
# ============================================================
# 3. Find all .afa files
# ============================================================

files <- list.files(
  alignment_dir,
  pattern = "\\.afa$",
  full.names = TRUE
)

cat("Number of alignments found:",
    length(files),
    "\n")


# ============================================================
# 4. Analyze every alignment
# ============================================================

results <- map(
  files,
  ~analyze_alignment(
    .x,
    min_window = min_window,
    max_window = max_window,
    min_conservation = min_conservation
  )
)


# ============================================================
# 5. Combine overall results
# ============================================================

orthogroup_conservation <-
  map_dfr(
    results,
    "summary"
  )


# ============================================================
# 6. Combine primer candidates
# ============================================================

primer_candidate_regions <-
  map_dfr(
    results,
    "candidates"
  )


# ============================================================
# 7. Rank orthogroups
# ============================================================

orthogroup_conservation <-
  orthogroup_conservation %>%
  arrange(
    desc(mean_pairwise_identity),
    desc(mean_position_conservation)
  )


# ============================================================
# 8. Rank primer candidates
# ============================================================

primer_candidate_regions <-
  primer_candidate_regions %>%
  arrange(
    desc(mean_conservation),
    desc(min_conservation)
  )


# ============================================================
# 9. Write output files
# ============================================================

write.csv(
  orthogroup_conservation,
  file.path(
    output_dir,
    "orthogroup_conservation.csv"
  ),
  row.names = FALSE
)


write.csv(
  primer_candidate_regions,
  file.path(
    output_dir,
    "primer_candidate_regions.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 10. Print summary
# ============================================================

cat("\n========================================\n")
cat("STEP 6 COMPLETE\n")
cat("========================================\n")

cat(
  "Orthogroups analyzed:",
  nrow(orthogroup_conservation),
  "\n"
)

cat(
  "Primer candidate regions:",
  nrow(primer_candidate_regions),
  "\n"
)

cat("\nTop conserved orthogroups:\n")

print(
  head(
    orthogroup_conservation,
    10
  )
)

cat("\nTop primer candidate regions:\n")

print(
  head(
    primer_candidate_regions,
    10
  )
)

cat("\nOutput files:\n")

cat(
  file.path(
    output_dir,
    "orthogroup_conservation.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "primer_candidate_regions.csv"
  ),
  "\n"
)

