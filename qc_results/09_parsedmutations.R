
# library -------

library(plyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(readxl, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(forcats, quietly = TRUE, warn.conflicts = FALSE)
library(gridExtra, quietly = TRUE, warn.conflicts = FALSE)
library(patchwork, quietly = TRUE, warn.conflicts = FALSE)
library(reshape2, quietly = TRUE, warn.conflicts = FALSE)

# Dir
dir_excel <- list.files(path = "qc_pangolin/files", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos -------
d_mut <- read_excel(dir_excel[1], sheet = 1)
# wide 1
df_mut_1 <- pivot_wider(d_mut, names_from = "lineage", values_from = "gene", id_cols = "amino acid", values_fill = "None")
write.table(df_mut_1, "df_mut_2.csv", sep = "\t", quote = F, row.names = F)
# write.table(df_mut, "df_mut.csv", sep = "\t", quote = F, row.names = F)

# wide 2
df_2_mut <- d_mut %>%
    group_by(lineage, `amino acid`) %>%
    summarise(n = dplyr::n(), .groups = "drop")
df_mut_2 <- pivot_wider(df_2_mut, names_from = "gene", values_from = "n", values_fill = 0)
write.table(df_mut_2, "df_mut.csv", sep = "\t", quote = F, row.names = F)
