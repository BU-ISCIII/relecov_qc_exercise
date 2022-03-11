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

qc_linajes <- read_excel(dir_excel[1], sheet = 7)

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

df_parsed_linajes <- as.data.frame(qc_parsed_linajes[, c(6:17)])

# linajes

df_linajes <- df_parsed_linajes[df_parsed_linajes$tipo == "linaje" & df_parsed_linajes$grupo != "control", ]
vector_samples <- paste0(rep("sample_", 3), rep(1:10))
colnames(df_linajes) <- c("grupo", "tipo", vector_samples)

df_linajes_control <- df_linajes[df_linajes$grupo == "control_nuevo", c(1, 3:12)]
df_linajes_lab <- df_linajes[df_linajes$grupo != "control_nuevo", c(1, 3:12)]

matrix_linajes <- matrix(0, ncol = 10, nrow = 40)
for (i in 1:10) {
  matrix_linajes[, i] <- df_linajes_control[, i + 1] == df_linajes_lab[, i + 1]
}

colnames(matrix_linajes) <- colnames(df_linajes[3:12])
rownames(matrix_linajes) <- NULL

df_resultados_linajes <- data.frame(id = df_linajes$grupo[2:41], matrix_linajes)

dim(df_resultados_linajes[, 2:11])

matrix_valores_0 <- matrix(0, ncol = 1, nrow = 40)
for (i in 1:40) {
  tabla_0 <- table(matrix_linajes[i, ])
  tabla_0 <- table(matrix_linajes[i, ])
  matrix_valores_0[i, 1] <- as.numeric(tabla_0[1])
}

table(matrix_valores_0[, 1])

# variantes

df_variantes <- df_parsed_linajes[df_parsed_linajes$tipo == "variante" & df_parsed_linajes$grupo != "control", ]
vector_samples <- paste0(rep("sample_", 3), rep(1:10))
colnames(df_variantes) <- c("grupo", "tipo", vector_samples)

df_variantes_control <- df_variantes[df_variantes$grupo == "control_nuevo", c(1, 3:12)]
df_variantes_lab <- df_variantes[df_variantes$grupo != "control_nuevo", c(1, 3:12)]

matrix_variantes <- matrix(0, ncol = 10, nrow = 40)
for (i in 1:10) {
  matrix_variantes[, i] <- df_variantes_control[, i + 1] == df_variantes_lab[, i + 1]
}

colnames(matrix_variantes) <- colnames(df_variantes[3:12])
rownames(matrix_variantes) <- NULL

df_resultados_variantes <- data.frame(id = df_variantes$grupo[2:41], matrix_variantes)

mm <- as.matrix(df_resultados_variantes[, 2:11])
table(mm)

# variantes y linajes

matrix_valores_0 <- matrix(0, ncol = 1, nrow = 40)
for (i in 1:40) {
  tabla_0 <- table(matrix_variantes[i, ])
  matrix_valores_0[i, 1] <- as.numeric(tabla_0[1])
}

table(matrix_valores_0[, 1])