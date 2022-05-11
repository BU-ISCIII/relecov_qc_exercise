
# library -------

library(plyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(readxl, quietly = TRUE, warn.conflicts = FALSE)
library(janitor, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(forcats, quietly = TRUE, warn.conflicts = FALSE)
library(gridExtra, quietly = TRUE, warn.conflicts = FALSE)
library(patchwork, quietly = TRUE, warn.conflicts = FALSE)
library(reshape2, quietly = TRUE, warn.conflicts = FALSE)

# data batch

temp <- list.files(path = "Data/Pangolin/Raw/viralrecon", pattern = "*.csv")

setwd("Data/Pangolin/Raw/viralrecon")

n <- c(16, 17, 18, 19)
nombres_data <- paste0("data_", n)
data_16$version <- rep("v3.1.16", length(data_16$taxon))
data_17$version <- rep("v3.1.17", length(data_17$taxon))
data_18$version <- rep("v3.1.18", length(data_18$taxon))
data_19$version <- rep("v3.1.19", length(data_19$taxon))

for (i in 1:length(temp)) assign(nombres_data[i], read.csv(temp[i], sep = "\t"))

setwd("/home/alberto.lema/Documents/Desarrollo/relecov_qc_exercise")

# Parsed data

data_lineages_raw <- rbind(data_16, data_17, data_18, data_19)

# eliminamos patrones

patron_1 <- "_NC_045512.2"
patron_2 <- "/ARTIC/medaka"

parsed_1 <- data_lineages_raw %>% separate(taxon, c("n1", "n2"), patron_1)
parsed_2 <- parsed_1 %>% separate(n1, c("n3", "n4"), patron_2)

# separamos para conseguir las muestras y los nombres de lab

id_parsed_3 <- grep("COD_", parsed_2$n3)
parsed_3 <- parsed_2[id_parsed_3, ]
parsed_4 <- parsed_3 %>% separate(n3, c("n5", "n6", "n7", "n8"), "_")

p1_parsed_4 <- parsed_4[is.na(parsed_4$n8) == T, ]
parsed_5 <- p1_parsed_4 %>% unite("grupo", c("n5", "n6"), sep = "_")
p1_parsed_5 <- parsed_5[, c(1, 2, 6, 7)]
colnames(p1_parsed_5) <- c("grupo", "muestra", "lineage", "version")


p2_parsed_4 <- parsed_4[is.na(parsed_4$n8) != T, ]
parsed_6 <- p2_parsed_4 %>% unite("grupo", c("n5", "n6", "n7"), sep = "_")
p1_parsed_6 <- parsed_6[, c(1, 2, 5, 6)]
colnames(p1_parsed_6) <- c("grupo", "muestra", "lineage", "version")

data_lineages_parsed <- rbind(p1_parsed_5, p1_parsed_6)

write.table(data_lineages_parsed, "data_lineages_parsed.csv", sep = "\t", row.names = F, quote = F)