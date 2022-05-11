
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

for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], sep = "\t"))

setwd("../../../..")

#