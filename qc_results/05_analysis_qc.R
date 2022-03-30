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
    coord_flip() +
    labs(y = "Number of laboratories", x = "", title = "Diagnosis protocol for SARS-CoV-2") +
    geom_text(stat = "count", aes(label = ..count..), vjust = 0.5, hjust = -1) +
    theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))
ggsave("Graficos/qc_barplot_diagnostico.png")

ggplot(subset(categorias_data, diagnostico_2 != "NA"), aes(diagnostico_2)) +
    geom_bar(fill = "#1F77B4") +
    labs(y = "Number of laboratories", x = "", title = "Diagnosis protocol for SARS-CoV-2", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
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
    labs(y = "Number of laboratories", x = "", title = "Libraries", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = 0.4, hjust = -1) +
    theme(axis.text.x = element_text(vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_librerias.png")

#### plot plataformas ----

ggplot(categorias_data, aes(platform_1)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Platform", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_plataforma.png")

ggplot(categorias_data, aes(platform_2, fill = platform_1)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Platform", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_plataforma_2.png")

#### plot enriquecimiento ----

ggplot(categorias_data, aes(enriquecimiento)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Enrichment", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_enrichment.png")

ggplot(categorias_data, aes(enriquecimiento)) +
    geom_bar(fill = "#1F77B4") +
    facet_grid(~platform_1) +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Enrichment") +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_enrichment_2.png")

#### plot genome ----

ggplot(categorias_data, aes(genome)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Reference genome", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_genome.png")

#### plot bioinformatica ----

ggplot(categorias_data, aes(bioinformatica)) +
    geom_bar(fill = "#1F77B4") +
    coord_flip() +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Bioinformatic protocol") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.8) +
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica.png")

ggplot(categorias_data, aes(bioinformatica)) +
    geom_bar(fill = "#1F77B4") +
    coord_flip() +
    facet_grid(~platform_1) +
    guides(fill = guide_legend(title = "Platform")) +
    labs(y = "Number of laboratories", x = "", title = "Bioinformatic protocol") +
    geom_text(stat = "count", aes(label = ..count..), vjust = 0.5, hjust = 2) +
    theme(axis.text.x = element_text(vjust = 1, hjust = 0, size = 12), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_2.png")

### datos bioinfo ----

qc_bioinfo <- read_excel(dir_excel[1], sheet = 8)

bioinfo_data <- data.frame(
    id = as.character(qc_bioinfo$ID),
    platform = factor(qc_bioinfo$plataformas_2, levels = c("Illumina", "Ion Torrent", "Nanopore")),
    preprocessing = as.character(qc_bioinfo$Preprocessing),
    mapping = qc_bioinfo$Mapping,
    assembly = qc_bioinfo$Assembly,
    variant = qc_bioinfo$Variant_Calling,
    consensus = qc_bioinfo$Consensus,
    lineage_software = qc_bioinfo$Linage_identification,
    pangolin_version = qc_bioinfo$pangolin_version)
)

#### plot preprocessing ----

ggplot(bioinfo_data, aes(preprocessing)) +
    geom_bar(fill = "#1F77B4") +
    facet_grid(~platform) +
    coord_flip() +
    guides(fill = guide_legend(title = "platform")) +
    labs(y = "Number of laboratories", x = "", title = "Preprocessing software") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.8) +
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_preprocessing.png")

#### plot mapping ----

ggplot(bioinfo_data, aes(mapping)) +
    geom_bar(fill = "#1F77B4") +
    facet_grid(~platform) +
    coord_flip() +
    guides(fill = guide_legend(title = "platform")) +
    labs(y = "Number of laboratories", x = "", title = "Mapping software") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.2) +
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_mapping.png")

#### plot assembly ----

ggplot(bioinfo_data, aes(assembly)) +
    geom_bar(fill = "#1F77B4") +
    facet_grid(~platform) +
    coord_flip() +
    guides(fill = guide_legend(title = "platform")) +
    labs(y = "Number of laboratories", x = "", title = "Assembly software") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.2) +
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_assembly.png")

#### plot consensus ----

ggplot(bioinfo_data, aes(consensus)) +
    geom_bar(fill = "#1F77B4") +
    facet_grid(~platform) +
    coord_flip() +
    guides(fill = guide_legend(title = "platform")) +
    labs(y = "Number of laboratories", x = "", title = "Consensus software") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.2) +
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_consensus.png")

#### plot lineages ----

ggplot(bioinfo_data, aes(lineage_software)) +
    geom_bar(fill = "#1F77B4") +
    coord_flip() +
    guides(fill = guide_legend(title = "platform")) +
    labs(y = "Number of laboratories", x = "", title = "Lineage software") +
    geom_text(stat = "count", aes(label = ..count..), hjust = -0.2) +
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1), axis.text.y = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_lineage.png")

ggplot(bioinfo_data, aes(pangolin_version)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "platform")) +
    labs(y = "Number of laboratories", x = "", title = "Pangolin version", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 12))
ggsave("Graficos/qc_barplot_bioinformatica_pangolin.png")


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

ggplot(ct_data, aes(x = muestra2, y = ct_N, fill = plataforma)) +
    geom_boxplot(show.legend = T) +
    guides(fill = guide_legend(title = "Platform")) +
    geom_point(data = ct_data[ct_data$id == "Control", ], aes(muestra2, ct), colour = "steelblue", shape = 23, width = 0.5, size = 2.5, height = 0.5) +
    labs(x = "", y = "Ct", title = "Valores de Ct") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))

ggplot(ct_data, aes(x = muestra2, y = ct_S, fill = plataforma)) +
    geom_boxplot(show.legend = T) +
    guides(fill = guide_legend(title = "Platform")) +
    geom_point(data = ct_data[ct_data$id == "Control", ], aes(muestra2, ct), colour = "steelblue", shape = 23, width = 0.5, size = 2.5, height = 0.5) +
    labs(x = "", y = "Ct", title = "Valores de Ct") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))


#### datos estadistica Ct ----

df_ct_data <- subset(ct_data, id != "Control")


subset(ct_data, plataforma == "Nanopore")

ct_t <- t(df_ct_data[i, c(5:8)])
id_t <- rep(as.character(t(df_ct_data[i, c(1)])), 4)
sample_t <- rep(as.character(t(df_ct_data[i, c(3)])), 4)
plataforma_t <- rep(as.character(t(df_ct_data[i, c(4)])), 4)

lista_ct <- list()
lista_id<- list()
lista_sample<- list()
lista_plataforma<- list()
for (i in 1:nrow(df_ct_data)) {
    #dataframes
    ct_t <- t(df_ct_data[i, c(5:8)])
    id_t <- rep(as.character(t(df_ct_data[i, c(1)])), 4)
    sample_t <- rep(as.character(t(df_ct_data[i, c(3)])), 4)
    plataforma_t<- rep(as.character(t(df_ct_data[i, c(4)])), 4)
    #listas
    lista_ct[[i]] <- data.frame(id = i, Ct = ct_t[, 1], tipo = c("PCR", "N", "ORF", "S"))
    lista_id[[i]] <- data.frame(id = i, id_v = id_t)
    lista_sample[[i]] <- data.frame(id = i, sample = sample_t)
    lista_plataforma[[i]] <- data.frame(id = i, plataforma = plataforma_t)
}

df_ct_data_t <- bind_rows(lista_ct, .id = "id"); row.names(df_ct_data_t) <- NULL
df_ct_id_t <- bind_rows(lista_id, .id = "id"); row.names(df_ct_id_t) <- NULL
df_ct_sample_t <- bind_rows(lista_sample, .id = "id"); row.names(df_ct_sample_t) <- NULL
df_ct_plataforma_t <- bind_rows(lista_plataforma, .id = "id"); row.names(df_ct_plataforma_t) <- NULL


ct_format_data <- cbind(
    df_ct_id_t[,2],
    df_ct_sample_t[,2],
    df_ct_plataforma_t[,2],
    df_ct_data_t[, c(2, 3)]
); colnames (ct_format_data)<- c("id", "sample", "plataforma", "Ct", "tipo")

# write.table(ct_format_data, "ct_format_data.csv", quote = F, row.names = F, sep = "\t")

##### Plot ct -----
ct_format_data$tipo <- factor(ct_format_data$tipo, levels = c("PCR", "ORF", "N", "S"))
ct_format_data$plataforma <- factor(ct_format_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))

ggplot(ct_format_data, aes(x = sample, y = Ct, fill = tipo)) +
    geom_boxplot(show.legend = T) +
    facet_grid(~plataforma) +
    labs(x = "", y = "Ct", title = "Ct values by gene") +
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
    plataforma2 = as.character(qc_estadistica$var_sequencing_platforms),
    qc10x = as.numeric(qc_estadistica$`var_QC>10x`),
    mean_depth = as.numeric(qc_estadistica$var_mean_depth_coverage),
    perc_Ns = as.numeric(qc_estadistica$var_n_Ns),
    variants_75 = as.numeric(qc_estadistica$var_number_variants_75),
    variants_effect = as.numeric(qc_estadistica$var_variantseffect),
    carreras = as.numeric(as.vector(qc_estadistica$`Number samples in run`)),
    read = as.numeric(as.vector(qc_estadistica$`Read lenght`)),
    layout = as.character(qc_estadistica$`Library layout`)
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

# NAs

# aa<- data.frame(estadistica_data[is.na(estadistica_data$perc_Ns) == T, c(1, 3, 4, 7)])
# write.table(aa, "ns_na.csv", sep = "\t", row.names = F, quote = F)

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

##### Plot variantes -----

data_variantes <- data.frame(estadistica_data[is.na(estadistica_data$variants_75) != T, c(1, 3, 4, 8)])
data_variantes$id <- factor(data_variantes$id, levels = unique(data_variantes$id))

# NAs

aa <- data.frame(estadistica_data[is.na(estadistica_data$variants_75) == T, c(1, 3, 4, 8)])
write.table(aa, "variant_na.csv", sep = "\t", row.names = F, quote = F)

##### Plot variantes illumina -----

ggplot(subset(data_variantes, plataforma == "Illumina"), aes(x = id, y = variants_75)) +
    geom_jitter(position = position_jitter(0.01), aes(color = muestra2)) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Variants (AF > 0.75) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_variantes_AF75_illumina.png")

##### Plot variantes Ion torrent -----

ggplot(subset(data_variantes, plataforma == "Ion Torrent"), aes(x = id, y = variants_75)) +
    geom_jitter(position = position_jitter(0.01), aes(color = muestra2)) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Variants (AF > 0.75) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_variantes_AF75_iontorrent.png")

##### Plot variantes nanopore no hay datos -----

data_efecto <- data.frame(estadistica_data[is.na(estadistica_data$variants_effect) != T, c(1, 3, 4, 9)])
data_efecto$id <- factor(data_efecto$id, levels = unique(data_efecto$id))

# NAs

# aa<- data.frame(estadistica_data[is.na(estadistica_data$variants_effect) == T, c(1, 3, 4, 9)])
# write.table(aa, "varianteffect_na.csv", sep = "\t", row.names = F, quote = F)

##### Plot variantes efecto -----

ggplot(subset(data_efecto, plataforma == "Illumina"), aes(x = id, y = variants_effect)) +
    geom_jitter(position = position_jitter(0.01), aes(color = muestra2)) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Variants effect (AF > 0.75) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_variantes_efecto_illumina.png")

ggplot(subset(data_efecto, plataforma == "Ion Torrent"), aes(x = id, y = variants_effect)) +
    geom_jitter(position = position_jitter(0.01), aes(color = muestra2)) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Variants effect (AF > 0.75) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_variantes_efecto_iontorrent.png")

ggplot(subset(data_efecto, plataforma == "Nanopore"), aes(x = id, y = variants_effect)) +
    geom_point(aes(color = muestra2)) +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Variants effect (AF > 0.75) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_variantes_efecto_nanopore.png")

#### Muestras por carrera -----

l_id_plataforma <- c(
    "COD_2103",
    "COD_2112",
    "COD_2117_2",
    "COD_2122",
    "COD_2123",
    "COD_2132",
    "COD_2106_2",
    "COD_2107",
    "COD_2108",
    "COD_2110",
    "COD_2113",
    "COD_2114",
    "COD_2116",
    "COD_2124",
    "COD_2124_2",
    "COD_2125",
    "COD_2126",
    "COD_2129",
    "COD_2134",
    "COD_2135",
    "COD_2137",
    "COD_2139",
    "COD_2141",
    "COD_2109",
    "COD_2111",
    "COD_2121",
    "COD_2131",
    "COD_2117",
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

carreras_data <- estadistica_data[!duplicated(estadistica_data$id), c(1, 3, 4, 5, 11, 12, 13)]
carreras_data$id <- factor(carreras_data$id, levels = l_id_plataforma)
n_carreras_data <- data.frame(carreras_data[is.na(carreras_data$carreras) != T, c(1, 3, 4, 5)])

# NAs

# aa<- data.frame(carreras_data[is.na(carreras_data$carreras) == T, c(1, 3, 4, 5)])
# write.table(aa, "carreras_na.csv", sep = "\t", row.names = F, quote = F)

##### Plot Muestras por carrera -----

ggplot(n_carreras_data, aes(x = id, y = carreras, fill = plataforma)) +
    geom_bar(stat = "identity") +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    geom_text(aes(label = carreras), vjust = -0.85, color = "black", size = 3) +
    labs(x = "", y = "samples / run", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )
ggsave("Graficos/qc_secuenciacion_carreras_plataforma.png")

ggplot(n_carreras_data, aes(x = id, y = carreras, fill = plataforma2)) +
    geom_bar(stat = "identity") +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    geom_text(aes(label = carreras), vjust = -0.85, color = "black", size = 3) +
    labs(x = "", y = "samples / run", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )
ggsave("Graficos/qc_secuenciacion_carreras_plataforma_2.png")

##### Plot read length -----

read_data <- data.frame(carreras_data[is.na(carreras_data$read) != T, c(1, 3, 4, 6, 7)])
read_data$id <- factor(read_data$id, levels = l_id_plataforma)

# aa<- data.frame(carreras_data[is.na(carreras_data$read) == T, c(1, 3, 4, 6)])
# write.table(aa, "read_na.csv", sep = "\t", row.names = F, quote = F)


ggplot(read_data, aes(x = id, y = read, fill = plataforma2)) +
    geom_col() +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    labs(x = "", y = "read length / sample", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )
ggsave("Graficos/qc_secuenciacion_reads_plataforma.png")


### datos linajes ----

### calculo de TP y FP

qc_parsed_linajes <- read_excel(dir_excel[1], sheet = 5)
df_parsed_linajes <- as.data.frame(qc_parsed_linajes[, c(3, 5:14)])

# linajes

# df_linajes_control <- df_parsed_linajes[df_parsed_linajes$grupo == "control", 2:11]
df_linajes_control<- c("B.1.1.7",
"B.1.351",
"A.28",
"B.1.621",
"P.1",
"AY.9.2",
"AY.43",
"AY.94",
"AY.94",
"AY.43")

df_linajes_lab <- df_parsed_linajes[df_parsed_linajes$grupo != "control", 2:11]

# calculamos los TP, FP, FN by sample

matrix_tasa<- matrix(0, ncol = 10, nrow = 40)
for (j in 1:ncol(df_linajes_lab)) {
    control<- df_linajes_control[j]
    muestra<- df_linajes_lab[, j]
        for (i in 1:length(muestra)) {
            if (control == muestra[i]) {
                matrix_tasa [i, j]<- "TP"
            } else if (muestra[i] == "None") {
                matrix_tasa [i, j]<- "FN"
            } else if (control != muestra[i]) {
                matrix_tasa [i, j]<- "FP"
            } 
        }   
}

table(matrix_tasa[,1])
table(matrix_tasa[,2])
table(matrix_tasa[,3])
table(matrix_tasa[,4])
table(matrix_tasa[,5])
table(matrix_tasa[,6])
table(matrix_tasa[,7])
table(matrix_tasa[,8])
table(matrix_tasa[,9])
table(matrix_tasa[,10])

matrix_linajes <- matrix(0, ncol = 10, nrow = 40)
for (i in 1:10) {
    control <- as.character(df_linajes_control[, i])
    sample <- as.character(df_linajes_lab[, i])
    if (control == sample) {
        matrix_linajes[,i]<- 
    } else {
        matrix_linajes[,i]<- 0 
    }
}

colnames(matrix_linajes) <- colnames(df_parsed_linajes[2:11])
rownames(matrix_linajes) <- NULL

df_resultados_linajes <- data.frame(id = df_parsed_linajes$grupo[2:41], matrix_linajes)

matrix_valores_0 <- matrix(0, ncol = 1, nrow = 40)
for (i in 1:40) {
    tabla_0 <- table(matrix_linajes[i, ])
    tabla_0 <- table(matrix_linajes[i, ])
    matrix_valores_0[i, 1] <- as.numeric(tabla_0[1])
}

table(matrix_valores_0[, 1])

# Sensibilidad y precision

qc_tasa <- read_excel(dir_excel[1], sheet = 7)
muestras<- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")
qc_tasa$samples<- as.character(rep(muestras, 2))
qc_tasa$samples<- factor (qc_tasa$samples, levels = c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10"))
qc_tasa$lineages<- factor (qc_tasa$lineages, levels = unique(df_linajes_control))
qc_tasa$tipo<- factor (qc_tasa$type, levels = c("Sensitivity", "Precision"))


# plot sensitivity & precision
ggplot(qc_tasa, aes(x = lineages, y = tasa, group = tipo)) + 
    geom_line(aes(color = type)) +
    facet_grid(~type) +
    guides(color = guide_legend(title = "Curves"), fill = guide_legend(title = "Platform")) +
    labs(x = "Lineages", y = "Sensitivity", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )

ggsave("Graficos/qc_lineages_sensitivity_precision_2.png")

# plot sensitivity & precision join
ggplot(qc_tasa, aes(x = lineages, y = tasa, group = tipo)) + 
    geom_line(aes(color = tipo)) +
    guides(color = guide_legend(title = "Curves"), fill = guide_legend(title = "Platform")) +
    labs(x = "Lineages", y = "Sensitivity", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )

ggsave("Graficos/qc_lineages_sensitivity_precision.png")

# plot sensitivity & precision smooth
ggplot(qc_tasa, aes(x = lineages, y = tasa, group = tipo)) + 
    geom_smooth(aes(color = tipo), method = "loess", se = F) +
    facet_grid(~type) +
    guides(color = guide_legend(title = "Curves"), fill = guide_legend(title = "Platform")) +
    labs(x = "Lineages", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )

ggsave("Graficos/qc_lineages_sensitivity_precision_smooth.png")

# mutaciones por muestra

matrix_mutaciones <- matrix(0, ncol = 2, nrow = 10)
for (i in 1:10) {
    matrix_mutaciones[i, 1]<- mean(mutaciones_data$variants_effect[mutaciones_data$muestra2 == unique(mutaciones_data$muestra2)[i]], na.rm = T)
    matrix_mutaciones[i, 2]<- median(mutaciones_data$variants_effect[mutaciones_data$muestra2 == unique(mutaciones_data$muestra2)[i]], na.rm = T)
}

qc_tasa$mutaciones<- round(as.numeric(rep (matrix_mutaciones[,1], 2)))

# plot sensitivity & mutations
data_sen<- qc_tasa[qc_tasa$type == "Sensitivity", ]

ggplot(data_sen, aes(x = lineages, y = tasa * 100)) + 
    geom_bar(aes(x = lineages, y = mutaciones), stat = "identity",fill = "steelblue") +
    geom_smooth(method = "loess", se = F, aes(group = tipo, fill = tipo)) +
    guides(fill = guide_legend(title = "Curve")) +
    labs(x = "Lineages", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )

ggplot(qc_tasa, aes(x = lineages, y = tasa * 100)) + 
    geom_bar(aes(x = lineages, y = mutaciones), stat = "identity",fill = "steelblue") +
    geom_smooth(method = "loess", se = F, aes(group = tipo, color = tipo)) +
    guides(fill = guide_legend(title = "Curve")) +
    labs(x = "Lineages", y = "", title = "") +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text()
    )

ggsave("Graficos/qc_lineages_sensitivity_precision_smooth_mutaciones.png")
