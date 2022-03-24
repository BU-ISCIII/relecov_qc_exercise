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

# datos modificados

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

#### Datos estadistica -----

qc_ct <- read_excel(dir_excel[1], sheet = 4)

nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")

ct_data <- data.frame(
    id = as.character(qc_ct$ID),
    muestra = as.character(qc_ct$`Sample ID`),
    muestra2 = as.character(rep(nombres_muestras, 41)),
    plataforma = as.character(qc_ct$plataforma),
    ct = as.numeric(qc_ct$`Valor Ct PCR`),
    ct_N = as.numeric(qc_ct$`Valor Ct N`),
    ct_ORF = as.numeric(qc_ct$`Valor Ct ORF`),
    ct_S = as.numeric(qc_ct$`Valor Ct S`)
)

ct_data$muestra2 <- factor(ct_data$muestra2, levels = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10"))

##### Plot ct -----

ggplot(ct_data, aes(x = muestra2, y = ct, fill = plataforma)) +
    geom_boxplot(show.legend = T) +
    guides(fill = guide_legend(title = "Platform")) +
    geom_point(data = ct_data[ct_data$id == "Control", ], aes(muestra2, ct), colour = "steelblue", shape = 23, width = 0.5, size = 2.5, height = 0.5) +
    labs(x = "", y = "Ct", title = "Valores de Ct") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_ct_muestras_control.png")

#### datos estadistica Ct ----

nombres_labs <- unique(ct_data$id)
nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")

lista_nombres <- list()
for (i in 1:41) {
    n_nombres_labs <- rep(nombres_labs[i], 41)
    lista_nombres[[i]] <- as.character(n_nombres_labs)
}
id_lab <- unlist(lista_nombres)

1681 - 41

lista_muestras <- list()
for (i in 1:10) {
    n_nombres_labs <- rep(nombres_muestras[i], 41)
    lista_muestras[[i]] <- as.character(n_nombres_labs)
}
id_muestra <- unlist(lista_muestras)

lista_ct <- list()
for (i in 1:nrow(ct_data)) {
    ct_t <- t(ct_data[i, 5:8])
    lista_ct[[i]] <- data.frame(id = i, Ct = ct_t[, 1], tipo = c("PCR", "N", "ORF", "S"))
}

df_ct <- bind_rows(lista_ct, .id = "id")

ct_format_data <- data.frame(
    id = id_muestra,
    df_ct[, c(2, 3)]
)

row.names(ct_format_data) <- NULL
ct_format_data$tipo <- factor(ct_format_data$tipo, levels = c("PCR", "ORF", "S", "N"))

lista_plataforma <- list()
for (i in 1:nrow(ct_data)) {
    nombre <- ct_data$plataforma[i]
    v_nombres <- rep(nombre, 4)
    lista_plataforma[[i]] <- v_nombres
}

id_plataforma <- unlist(lista_plataforma)

ct_format_data$plataforma <- id_plataforma
ct_format_data$id <- factor(ct_format_data$id, levels = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10"))

##### Plot ct -----

ggplot(ct_format_data, aes(x = id, y = Ct, fill = tipo)) +
    geom_boxplot(show.legend = T) +
    facet_grid(~plataforma) +
    labs(x = "", y = "Ct", title = "Valores de Ct") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_ct_plataforma.png")

#### Datos estadistica -----

qc_estadistica <- read_excel(dir_excel[1], sheet = 3)

nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")

estadistica_data <- data.frame(
    id = as.character(qc_estadistica$ID),
    muestra = as.character(qc_estadistica$`Sample ID`),
    muestra2 = as.character(rep(nombres_muestras, 40)),
    plataforma = as.character(qc_estadistica$plataforma),
    qc10x = as.numeric(qc_estadistica$`var_QC>10x`),
    mean_depth = as.numeric(qc_estadistica$var_mean_depth_coverage),
    perc_Ns = as.numeric(qc_estadistica$var_n_Ns),
    variants_75 = as.numeric(qc_estadistica$var_number_variants_75),
    variants_effect = as.numeric(qc_estadistica$var_variantseffect)
)

estadistica_data$muestra2 <- factor(estadistica_data$muestra2, levels = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10"))
estadistica_data$plataforma <- factor(estadistica_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))

levels_id <- c(
    "COD_2103",
    "COD_2106_2",
    "COD_2107",
    "COD_2108",
    "COD_2109",
    "COD_2110",
    "COD_2111",
    "COD_2112",
    "COD_2113",
    "COD_2114",
    "COD_2116",
    "COD_2117",
    "COD_2117_2",
    "COD_2121",
    "COD_2122",
    "COD_2123",
    "COD_2124",
    "COD_2124_2",
    "COD_2125",
    "COD_2126",
    "COD_2129",
    "COD_2131",
    "COD_2132",
    "COD_2134",
    "COD_2135",
    "COD_2137",
    "COD_2139",
    "COD_2141",
    "COD_2102",
    "COD_2104",
    "COD_2105",
    "COD_2115",
    "COD_2119",
    "COD_2120",
    "COD_2127",
    "COD_2136",
    "COD_2143",
    "COD_2106",
    "COD_2107_2",
    "COD_2140"
)

estadistica_data$id <- factor(estadistica_data$id, levels = levels_id)

##### Plot depth -----

ggplot(subset(estadistica_data, mean_depth > 5), aes(x = id, y = log10(mean_depth), fill = plataforma)) +
    geom_boxplot() +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    labs(x = "", y = "log10(coverage (mean depth)) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_coverage_platform_nosamples.png")

##### Plot % genoma 10x -----
# aa<- estadistica_data[is.na(estadistica_data$qc10x), c(1, 3, 4, 5)]
# write.table(aa, "qc10_na.csv", sep = "\t", row.names = F, quote = F)


ggplot(estadistica_data, aes(x = id, y = qc10x, fill = plataforma)) +
    geom_boxplot() +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    geom_jitter(position = position_jitter(0.001), aes(color = muestra2)) +
    labs(x = "", y = "genome % > 10x / sample", title = "", size = 10) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_genoma_10x.png")

ggplot(estadistica_data, aes(x = id, y = qc10x, fill = plataforma)) +
    geom_boxplot() +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    labs(x = "", y = "genome % > 10x / sample", title = "", size = 10) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_genoma_10x_sin samples.png")

##### Plot Ns illumina -----

data_n <- data.frame(estadistica_data[is.na(estadistica_data$perc_Ns) != T, c(1, 3, 4, 7)])
data_n$id <- factor(data_n$id, levels = unique(data_n$id))

ggplot(subset(data_n, plataforma == "Illumina"), aes(x = factor(id), y = perc_Ns, fill = muestra2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Ns % / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_Ns_illumina.png")

##### Plot Ns Ion torrent -----

ggplot(subset(data_n, plataforma == "Ion Torrent"), aes(x = factor(id), y = perc_Ns, fill = muestra2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Ns % / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_Ns_iontorrent.png")

##### Plot Ns Nanopore -----

ggplot(subset(data_n, plataforma == "Nanopore"), aes(x = factor(id), y = perc_Ns, fill = muestra2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Ns % / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_Ns_nanopore.png")