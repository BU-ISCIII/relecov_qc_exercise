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
library(viridis, quietly = TRUE, warn.conflicts = FALSE)

### Excel ----

dir_excel <- list.files(path = "Data", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos Fechas ----

qc_fechas <- read_excel(dir_excel[1], sheet = 2)

# interesante las fechas de envio, recepcion y emision

fechas_data <- data.frame(
    id = as.character(qc_fechas$ID),
    envio = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de envio`)), date_system = "modern"),
    recepcion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de recepcion`)), date_system = "modern"),
    secuenciacion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de secuenciacion`)), date_system = "modern"),
    emision = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de emision de resultados`)), date_system = "modern"),
    ejecucion = as.numeric(qc_fechas$`Tiempo de ejecucion`)
)

#### plot tiempo de ejecucion ----

ggplot(fechas_data, aes(
    x = as.Date(fechas_data$recepcion), xend = as.Date(fechas_data$secuenciacion),
    y = id, yend = id, color = "#4a8abe"
)) +
    geom_segment(size = 3, show.legend = F) +
    geom_text(aes(label = fechas_data$ejecucion), position = position_dodge(width = 1), hjust = -1, color = "black", size = 2.5) +
    labs(x = "Execution time (days)", y = "")
ggsave("Graficos/qc_tiempo_ejecucion.png")

#### boxplot ejecucion ----

ggplot(fechas_data, aes(y = ejecucion)) +
    geom_boxplot() +
    labs(y = "Execution time (days)", x = "")
ggsave("Graficos/qc_boxplot_ejecucion.png")

### datos categorias ----

qc_categorias <- read_excel(dir_excel[1], sheet = 1)

categorias_data <- data.frame(
    id = as.character(qc_categorias$ID),
    platform_1 = factor(qc_categorias$sequencing_platforms_1, levels = c("Illumina", "Nanopore", "Ion Torrent")),
    platform_2 = as.character(qc_categorias$sequencing_platforms_2),
    librerias = as.character(qc_categorias$libraries_2)
)

#### levels librerias ----

categorias_data$librerias <- factor(categorias_data$librerias, levels = c(c(
    "ABL DeepChek NGS kit",
    "Illumina DNA Prep Tagmentation",
    "Illumina COVIDSeq Test kit",
    "NEBNext® Ultra™ II FS DNA Library Prep Kit for Illumina",
    "NEBNext® ARTIC SARS-CoV-2 FS kit for Illumina",
    "Ion Xpress kit",
    "Ion AmpliSeq Kit for Chef DL8 kit",
    "NEBNext® Fast DNA Library Prep Set for Ion Torrent™ kit",
    "ViroKey SQ FLEX SARS-CoV-2 Genotyping Assay (RUO) for Ion Torrent",
    "Ion Code kit",
    "Ion AmpliSeq™ SARS-CoV-2 Insight Research Assay - GS Manual",
    "Oxford Nanopore Sequencing Kit"
)))

#### plot librerias ----

count_librerias <- as.numeric(table(categorias_data$librerias))

ggplot(subset(categorias_data, !is.na(librerias)), aes(librerias, fill = platform_1)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "", x = "") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_librerias.png")