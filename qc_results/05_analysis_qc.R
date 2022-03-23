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
    platform_1 = factor(qc_categorias$sequencing_platforms_1, levels = c("Illumina", "Ion Torrent", "Nanopore")),
    platform_2 = as.character(qc_categorias$sequencing_platforms_2),
    librerias = as.character(qc_categorias$libraries_2),
    diagnostico_1 = qc_categorias$protocolo_diagnostico_2,
    diagnostico_2 = qc_categorias$protocolo_diagnostico_4,
    enriquecimiento = qc_categorias$enrichment,
    genome = qc_categorias$reference_genome,
    bioinformatica = qc_categorias$bioinformatic_protocol,
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
    labs(y = "Number of laboratories", x = "", title = "Diagnosis protocol for SARS-CoV-2") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_diagnostico.png")

ggplot(subset(categorias_data, diagnostico_2 != "NA"), aes(diagnostico_2)) +
    geom_bar() +
    labs(y = "Number of laboratories", x = "", title = "Diagnosis protocol for SARS-CoV-2") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_diagnostico_2.png")

ggplot(subset(categorias_data, diagnostico_1 != "NA"), aes(diagnostico_1)) +
    geom_bar() +
    facet_grid(~platform_1) +
    labs(y = "Number of laboratories", x = "", title = "Diagnosis protocol for SARS-CoV-2") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6))
ggsave("Graficos/qc_barplot_diagnostico_3.png")

#### plot librerias ----

ggplot(subset(categorias_data, !is.na(librerias)), aes(fct_reorder(categorias_data$librerias, categorias_data$values), fill = platform_1)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    coord_flip() +
    labs(y = "Number of laboratories", x = "", title = "Libraries") +
    geom_text(stat = "count", aes(label = ..count..), vjust = 1, hjust = 2) +
    theme(axis.text.x = element_text(vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_librerias.png")

#### plot plataformas ----

ggplot(categorias_data, aes(platform_1)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Platform") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_plataforma.png")

ggplot(categorias_data, aes(platform_2, fill = platform_1)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Platform") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_plataforma_2.png")

#### plot enriquecimiento ----

ggplot(categorias_data, aes(enriquecimiento)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Enrichment") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_enrichment.png")

ggplot(categorias_data, aes(enriquecimiento)) +
    geom_bar() +
    facet_grid(~platform_1) +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Enrichment") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_enrichment_2.png")

#### plot genome ----

ggplot(categorias_data, aes(genome)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Reference genome") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_genome.png")

#### plot bioinformatica ----

ggplot(categorias_data, aes(bioinformatica)) +
    geom_bar() +
    coord_flip() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Bioinformatic protocol") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -2) +
    theme(axis.text.x = element_text(size = 6, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica.png")

ggplot(categorias_data, aes(bioinformatica)) +
    geom_bar() +
    facet_grid(~platform_1) +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Bioinformatic protocol") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, size = 6, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica_2.png")