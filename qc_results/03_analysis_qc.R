#!/usr/bin/env Rscript

# library -------

library(plyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(readxl, quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)
library(readODS, quietly = TRUE, warn.conflicts = FALSE)
library(readr, quietly = TRUE, warn.conflicts = FALSE)
library(janitor, quietly = TRUE, warn.conflicts = FALSE)
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)

### Excel ----

dir_excel <- list.files(path = "Data", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos linajes ----

qc_linajes <- read_excel(dir_excel[1], sheet = 4)

linajes_data <- data.frame(
  id = as.character(qc_linajes$ID),
  variantes = as.character(qc_linajes$Variante),
  linajes = as.character(qc_linajes$`Asignacion de Linaje`),
  version = as.character(qc_linajes$`Version Pango`)
)

lista_linajes <- list()
for (i in 1:length(unique(as.character(linajes_data$id)))) {
  id_lab <- unique(as.character(linajes_data$id))
  df_lab <- linajes_data[linajes_data$id == id_lab[i], ]
  new_id <- as.character(rep(id_lab[i], 3))
  id_bind <- seq(1, 2, 3)
  tipo <- c("variantes", "linajes", "version Pango")

  # Creamos el dataframe
  n_df_lab <- t(df_lab[, 2:4])
  n_2_df_lab <- cbind(n_df_lab, new_id, tipo, id_bind)
  colnames(n_2_df_lab) <- c("#1", "#2", "#3", "#4", "#5", "#6", "#7", "#8", "#9", "#10", "id_lab", "tipo", "id_bind")
  rownames(n_2_df_lab) <- NULL

  # Creamos la lista
  lista_linajes[[i]] <- as.data.frame(n_2_df_lab)
}

df_linajes <- do.call(rbind.data.frame, lista_linajes)

# write.table(df_linajes, "Data/linajes_qc_mod.csv", row.names = F, col.names = T, quote = F, sep = "\t")

### calculo de TP y FP

qc_parsed_linajes <- read_excel(dir_excel[1], sheet = 8)

df_parsed_linajes <- qc_parsed_linajes[,c(6:17)]

# linajes

df_linajes<- df_parsed_linajes[df_parsed_linajes$tipo == "linaje", ]



