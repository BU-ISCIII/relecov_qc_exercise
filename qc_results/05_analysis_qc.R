#!/usr/bin/env Rscript

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
    values = qc_categorias$values,
    platform_1 = factor(qc_categorias$sequencing_platforms_1, levels = c("Illumina", "Nanopore", "Ion Torrent")),
    platform_2 = as.character(qc_categorias$sequencing_platforms_2),
    librerias = as.character(qc_categorias$libraries_2),
    diagnostico_1 = qc_categorias$protocolo_diagnostico_2,
    prueba = qc_categorias$"Library layout",
    comercial = factor(qc_categorias$comercial, levels = c("Thermo Fisher", "Seegene", "Vircell", "None"))
)

#### plot diagnostico ----


categorias_data$diagnostico_1 <- fct_relevel(categorias_data$diagnostico_1, c(
    "RT-PCR 2019-nCoV Assay kit",
    "RT-PCR 2019-nCoV RUO kit",
    "RT-PCR COVID-19 CE-IVD kit",
    "RT-PCR Allplex SARS-Cov-2-Master Assay",
    "RT-PCR AllplexTM SARS-CoV-2/FluA/FluB/RSV kit",
    "RT-PCR Multiplex SARS-CoV-2 kit",
    "RT-PCR Vircell Multiplex SARS-CoV-2 kit",
    "Commercial detection kit",
    "in house RT-PCR",
    "NA",
    "Sequencing"
))

ggplot(subset(categorias_data, diagnostico_1 != "NA"), aes(diagnostico_1, fill = comercial)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Commercial")) +
    labs(y = "", x = "") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_diagnostico.png")

#### plot librerias ----

ggplot(subset(categorias_data, !is.na(librerias)), aes(fct_reorder(categorias_data$librerias, categorias_data$values), fill = platform_1)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "", x = "") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_librerias.png")