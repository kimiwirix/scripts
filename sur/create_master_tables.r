# setwd("~/lab/exp/2026/today7/")
library(tidyverse)

#' First we create a master table
Tab.exp <- read_tsv("data/abundance_table_open/feature-table-open-matched.tsv",
                comment = "")
colnames(Tab.exp)[1] <- "Strain"
Tab.exp


Tab.inoc <- read_tsv("data/inocs_table/feature-table-open-ensambles.tsv",
                     skip = 1, comment = "")
colnames(Tab.inoc)[1] <- "Strain"
Tab.inoc <- Tab.inoc %>%
  filter(Strain %in% c("CH111", "CH90", "CH161d", "CH29", "CH99b", "CH149a",
                         "CH154a", "CH23", "CH447", "CH450")) %>%
  select(Strain, c(paste0("CC000", 1:9), paste0("CC00",10:32)))
Tab.inoc


Tab <- Tab.inoc %>%
  full_join(Tab.exp, by = "Strain")
Tab


# Now the metadata
Meta.exp  <- read.table("CC_dbs/metadata_db.tsv", row.names = 1) %>%
  as_tibble()
Meta.exp

Meta.inoc <- tibble(community = NA,
                    techrep = "A",
                    temp = NA,
                    timepoint = 1,
                    repbio = 0,
                    label = colnames(Tab.inoc)[-1],
                    label_final = colnames(Tab.inoc)[-1],
                    batch = NA,
                    hrs = 0) %>%
  mutate(community = paste0("C", str_remove(label, "^CC[0]+")))
Meta.inoc

Meta <- Meta.inoc %>%
  bind_rows(Meta.exp)
Meta

write_tsv(Tab, "data/counts_all.tsv")
write_tsv(Meta, "data/meta_all.tsv")

#' First plan is to model abundance as a function of temperature
#' and community. 