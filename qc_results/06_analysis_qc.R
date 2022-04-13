
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

### Excel ----

dir_excel <- list.files(path = "Data", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

# levels COD_lab
rep_level<- c(
"COD_2106_2",
"COD_2117_2",
"COD_2124_2",
"COD_2107_2"
)

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
    "COD_2101",
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

### datos Fechas ----

qc_fechas <- read_excel(dir_excel[1], sheet = 2)

fechas_data <- data.frame(
    id = as.character(qc_fechas$ID),
    envio = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de envio`)), date_system = "modern"),
    recepcion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de recepcion`)), date_system = "modern"),
    secuenciacion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de secuenciacion`)), date_system = "modern"),
    emision = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de emision de resultados`)), date_system = "modern"),
    ejecucion = as.numeric(qc_fechas$`Tiempo de ejecucion`)
)

#### plot tiempo de ejecucion ----

# Sys.setlocale("LC_TIME", "English")
# format(Sys.Date(), format = "%Y-%b-%d")


subset_fechas_data<- subset(fechas_data, !(id %in% rep_level))
ggplot(subset_fechas_data, aes(
    x = as.Date(recepcion), xend = as.Date(secuenciacion),
    y = id, yend = id, color = "#4a8abe"
)) +
    geom_segment(size = 3, show.legend = F) +
    geom_text(aes(label = ejecucion), position = position_dodge(width = 1), hjust = 1.2, color = "black", size = 5) +
    labs(x = "Tiempo de ejecución (días)", y = "") +
    theme(
        text = element_text(size = 23),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.9)
    )
ggsave("Graficos/qc_tiempo_ejecucion.png")

# Datos reads

qc_reads <- read_excel(dir_excel[1], sheet = 4)
# nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")
nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")

reads_data <- data.frame(
    id = as.character(qc_reads$ID),
    muestra = as.character(qc_reads$`Sample ID`),
    muestra2 = as.character(rep(nombres_muestras, 41)),
    plataforma = as.character(qc_reads$plataforma),
    plataforma2 = as.character(qc_reads$var_sequencing_platforms),
    reads = as.numeric(qc_reads$var_readcount),
    tipo = factor(qc_reads$tipo, levels = c("pre-trimming", "post-trimming"))
)

reads_data$plataforma <- factor(reads_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))
reads_data$muestra2 <- factor(reads_data$muestra2, levels = c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10"))
reads_data$id <- factor(reads_data$id, levels = levels_id)

# plot reads filtered

ggplot(reads_data, aes(x = muestra2, y = log10(reads), fill = tipo)) +
    geom_boxplot() +
    guides(color = guide_legend(title = ""), fill = guide_legend(title = "Filtro")) +
    facet_grid(~plataforma) +
    labs(x = "", y = "log10 (lecturas) / muestra") +
    theme(
        text = element_text(size = 22),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45)
    )
ggsave("Graficos/qc_resultados_read_sample_platform.png")

# new virus, host data

qc_virushost <- read_excel(dir_excel[1], sheet = 5)
#nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")
nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")

virushost_data <- data.frame(
    id = as.character(qc_virushost$ID),
    muestra = as.character(qc_virushost$`Sample ID`),
    muestra2 = factor(rep(nombres_muestras, 41), levels = nombres_muestras),
    plataforma = as.character(qc_virushost$plataforma),
    plataforma2 = as.character(qc_virushost$var_sequencing_platforms),
    porcentaje = as.numeric(qc_virushost$percentage),
    tipo = factor(qc_virushost$tipo, levels = c("host", "virus", "unmapped"))
)

virushost_data$plataforma <- factor(virushost_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))
virushost_data$id <- factor(virushost_data$id, levels = levels_id)

# plot host, virus, unmapped

virushost_data$tipo <- revalue(virushost_data$tipo, c("host" = "Huésped", "virus" = "Virus", "unmapped" = "No mapeado"))

ggplot(virushost_data, aes(x = muestra2, y = porcentaje)) +
    geom_boxplot(fill = "#1F77B4") +
    facet_grid(plataforma ~ tipo) +
    labs(x = "", y = "%", title = "") +
    theme(
        text = element_text(size = 22),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45)
    )
ggsave("Graficos/qc_resultados_hostvirusunmapped_plataforma.png")

#### Datos depth -----

qc_estadistica <- read_excel(dir_excel[1], sheet = 3)
#nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")
nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")

estadistica_data <- data.frame(
    id = as.character(qc_estadistica$ID),
    muestra = as.character(qc_estadistica$`Sample ID`),
    muestra2 = as.character(rep(nombres_muestras, 41)),
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

estadistica_data$muestra2 <- factor(estadistica_data$muestra2, levels = nombres_muestras)
estadistica_data$plataforma <- factor(estadistica_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))
estadistica_data$id <- factor(estadistica_data$id, levels = levels_id)

##### Plot depth -----

ggplot(subset(estadistica_data, mean_depth > 5), aes(x = muestra2, y = log10(mean_depth))) +
    geom_boxplot(fill = "#1F77B4") +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = ""), fill = guide_legend(title = "")) +
    labs(x = "", y = "log10(cobertura (media de profundidad)) / muestra", title = "") +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45))
ggsave("Graficos/qc_resultados_coverage_plataforma.png")

##### Plot %depth -----

ggplot(estadistica_data, aes(x = muestra2, y = qc10x)) +
    geom_boxplot(fill = "#1F77B4") +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = ""), fill = guide_legend(title = "")) +
    labs(x = "", y = " Porcentaje de genoma > 10x / muestra", title = "") +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45))
ggsave("Graficos/qc_resultados_porcentajecoverage_plataforma.png")

### datos categorias ----

qc_categorias <- read_excel(dir_excel[1], sheet = 1)

categorias_data <- data.frame(
    id = as.character(qc_categorias$ID),
    platform_1 = factor(qc_categorias$sequencing_platforms_1, levels = c("Illumina", "Ion Torrent", "Nanopore")),
    platform_2 = as.character(qc_categorias$sequencing_platforms_2),
    genome = qc_categorias$reference_genome,
    bioinformatica = qc_categorias$bioinformatic_protocol
)

#### plot genome ----

ggplot(categorias_data, aes(genome)) +
    geom_bar(fill = "#1F77B4") +
    coord_flip() +
    guides(fill = guide_legend(title = "")) +
    labs(y = "Número de laboratorios", x = "", title = "", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = 1, size = 6.5, hjust = 2) +
    theme(
    text = element_text(size = 23),
    axis.text.x = element_text(), axis.text.y = element_text(),
    legend.title = element_text(),
    legend.text = element_text())
ggsave("Graficos/qc_barplot_genome.png")

# plot carreras

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
    "COD_2101",
    "COD_2106",
    "COD_2107_2",
    "COD_2140"
)

carreras_data <- estadistica_data[!duplicated(estadistica_data$id), c(1, 3, 4, 5, 11, 12, 13)]
carreras_data$id <- factor(carreras_data$id, levels = l_id_plataforma)
n_carreras_data <- data.frame(carreras_data[is.na(carreras_data$carreras) != T, c(1, 3, 4, 5)])

##### Plot Muestras por carrera -----

ggplot(n_carreras_data, aes(x = id, y = carreras, fill = plataforma2)) +
    geom_bar(stat = "identity") +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Instrumento")) +
    geom_text(aes(label = carreras), color = "black", vjust = -0.5, size = 10) +
    labs(x = "", y = "Muestras / Carrera", title = "") +
    theme(
        text = element_text(size = 50),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text()
    )

ggsave("Graficos/qc_secuenciacion_carreras_plataforma.png", width = 80, height = 80, dpi = 300, units = c("cm"))

##### Plot read length -----

read_data <- data.frame(carreras_data[is.na(carreras_data$read) != T, c(1, 3, 4, 6, 7)])
read_data$id <- factor(read_data$id, levels = l_id_plataforma)


ggplot(read_data, aes(x = id, y = read, fill = plataforma2)) +
    geom_col() +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Instrumento")) +
    labs(x = "", y = "Longitud lectura / muestra", title = "") +
    theme(
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text()
    )
#ggsave("Graficos/qc_secuenciacion_reads_plataforma.png", width = 80, height = 80, dpi = 300, units = c("cm"))
ggsave("Graficos/qc_secuenciacion_reads_plataforma.png")

#### plot bioinformatica ----

ggplot(categorias_data, aes(bioinformatica, fill = platform_1)) +
    geom_bar() +
    coord_flip() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", aes(label = ..count..), vjust = 0.5, hjust = 1.5, , color = "black", size = 6.5) +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica.png")


### datos bioinfo ----

qc_bioinfo <- read_excel(dir_excel[1], sheet = 13)

bioinfo_data <- data.frame(
    id = as.character(qc_bioinfo$ID),
    platform = factor(qc_bioinfo$plataformas_2, levels = c("Illumina", "Ion Torrent", "Nanopore")),
    preprocessing = as.character(qc_bioinfo$Preprocessing),
    bioinformatic = qc_bioinfo$bioinformatic_protocol,
    mapping = qc_bioinfo$Mapping,
    assembly = qc_bioinfo$Assembly,
    variant = qc_bioinfo$Variant_Calling,
    consensus = qc_bioinfo$Consensus,
    lineage_software = qc_bioinfo$Linage_identification,
    pangolin_version = qc_bioinfo$pangolin_version)
)

#### plot preprocessing ----

ggplot(bioinfo_data, aes(preprocessing, fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count))) +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica_preprocessing_plat.png")

#### plot mapping ----

ggplot(bioinfo_data, aes(mapping, fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count))) +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica_mapping_plat.png")

#### plot assembly ----

ggplot(bioinfo_data, aes(assembly,  fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count))) +
    theme(
    text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica_assembly_plat.png")

#### plot variant callers ----

ggplot(bioinfo_data, aes(variant, fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count))) +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica_variants_plat.png")

#### plot consensus ----

ggplot(bioinfo_data, aes(consensus, fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 12) +
    theme(
    text = element_text(size = 30),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_barplot_bioinformatica_consensus_plat.png", width = 25, height = 25)

# pangolin versions

ggplot(bioinfo_data, aes(pangolin_version)) +
    geom_bar(fill = "#1F77B4") +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "", size = 12) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 6.5) +
    theme(
    text = element_text(size = 22),
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45))
ggsave("Graficos/qc_barplot_bioinformatica_pangolin.png")

##### variantes data -----

qc_variants <- read_excel(dir_excel[1], sheet = 7)

#nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")
nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")

variants_data <- data.frame(
    id = as.character(qc_variants$ID),
    muestra = as.character(rep(nombres_muestras, 41)),
    plataforma = as.character(qc_variants$plataforma),
    plataforma2 = as.character(qc_variants$var_sequencing_platforms),
    software = as.character(qc_variants$software),
    variants = as.numeric(qc_variants$variants),
    tipo = as.character(qc_variants$tipo)
)

variants_data$muestra <- factor(variants_data$muestra, levels = nombres_muestras)
variants_data$plataforma <- factor(variants_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))
variants_data$tipo <- revalue(variants_data$tipo, c("Variants (AF > 0.75)" = "Mutaciones (FA > 0.75)", "Variants with effect" = "Mutaciones con efecto"))
variants_data$id <- factor(variants_data$id, levels = levels_id)

##### Plot variantes illumina & ion torrent -----

subset_variants_data<- subset(variants_data, variants < 80)

ggplot(subset_variants_data, aes(x = muestra, y = variants, fill = tipo)) +
    geom_violin() +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Mutaciones")) +
    labs(x = "", y = "Mutaciones / muestra", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    text = element_text(size = 22))
ggsave("Graficos/qc_resultados_variantes.png", dpi = 500, units = "cm", width = 40, height = 40)

# variantes software

p1<- ggplot(subset(variants_data, plataforma == "Illumina"), aes(x = software, y = variants)) +
    geom_bar(aes(), stat = "identity", position = position_dodge()) +
    geom_boxplot(fill = "#1F77B4", outlier.colour="red") +
    facet_grid(~tipo) +
    scale_y_continuous(
        name = "Mutaciones",
        sec.axis = sec_axis(trans=~./24, name="")
    ) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Mutaciones / software", title = "Illumina software") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    text = element_text(size = 15))

p2<- ggplot(subset(variants_data, plataforma == "Ion Torrent"), aes(x = software, y = variants)) +
    geom_bar(aes(), stat = "identity", position = position_dodge()) +
    geom_boxplot(fill = "#1F77B4", outlier.colour="red") +
    facet_grid(~tipo) +
    scale_y_continuous(
        name = "",
        sec.axis = sec_axis(trans=~./25, name="Software")
    ) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "", title = "Ion Torrent software") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    text = element_text(size = 15))

p1 + p2
ggsave("Graficos/qc_variantes_software.png", dpi = 500, units = "cm", width = 30, height = 30)

#### plot variant callers ----

ggplot(bioinfo_data, aes(variant, fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count))) +
    theme(
    text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("Graficos/qc_variantes_software.png", dpi = 500, units = "cm", width = 25, height = 25)


### calculo de TP y FP - QC

# linajes

qc_parsed_linajes <- read_excel(dir_excel[1], sheet = 9
df_parsed_linajes <- as.data.frame(qc_parsed_linajes[, c(3, 5:14)])

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

matrix_tasa<- matrix(0, ncol = 10, nrow = 41)
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

matrix_tasa<- matrix(0, ncol = 10, nrow = 41)
for (j in 1:ncol(df_linajes_lab)) {
    control<- df_linajes_control[j]
    muestra<- df_linajes_lab[, j]
        for (i in 1:length(muestra)) {
            if (control == muestra[i]) {
                matrix_tasa [i, j]<- "Bien"
            } else if (muestra[i] == "None") {
                matrix_tasa [i, j]<- "Mal"
            } else if (control != muestra[i]) {
                matrix_tasa [i, j]<- "Mal"
            } 
        }   
}


matrix_valores_1 <- matrix(0, ncol = 1, nrow = 41)
for (i in 1:41) {
    tabla_1 <- table(matrix_tasa[i, ])
    matrix_valores_1[i, 1] <- as.numeric(tabla_1[1])
}

df_aciertos_lin<- data.frame(
    id = df_parsed_linajes$grupo[-1],
    aciertos = matrix_valores_1[,1]
)


# variantes

qc_parsed_variantes <- read_excel(dir_excel[1], sheet = 10)
df_parsed_variantes <- as.data.frame(qc_parsed_variantes[, c(1, 3:12)])

# df_linajes_control <- df_parsed_linajes[df_parsed_linajes$grupo == "control", 2:11]
df_variantes_control<- c("Alfa",
"Beta",
"None",
"Mu",
"Gamma",
"Delta",
"Delta",
"Delta",
"Delta",
"Delta")

df_variantes_lab <- df_parsed_variantes[df_parsed_variantes$grupo != "control", 2:11]

# calculamos los TP, FP, FN by sample

matrix_tasa<- matrix(0, ncol = 10, nrow = 41)
for (j in 1:ncol(df_variantes_lab)) {
    control<- df_variantes_control[j]
    muestra<- df_variantes_lab[, j]
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

matrix_tasa<- matrix(0, ncol = 10, nrow = 41)
for (j in 1:ncol(df_variantes_lab)) {
    control<- df_variantes_control[j]
    muestra<- df_variantes_lab[, j]
        for (i in 1:length(muestra)) {
            if (control == muestra[i]) {
                matrix_tasa [i, j]<- "Bien"
            } else if (muestra[i] == "None") {
                matrix_tasa [i, j]<- "Mal"
            } else if (control != muestra[i]) {
                matrix_tasa [i, j]<- "Mal"
            } 
        }   
}


matrix_valores_1 <- matrix(0, ncol = 1, nrow = 41)
for (i in 1:41) {
    tabla_1 <- table(matrix_tasa[i, ])
    matrix_valores_1[i, 1] <- as.numeric(tabla_1[1])
}

df_aciertos_variantes<- data.frame(
    id = df_parsed_linajes$grupo[-1],
    aciertos = matrix_valores_1[,1]
)

# write.table(df_aciertos, "df_aciertos.csv", row.names = F, quote = F, sep = "\t")

### calculo de TP y FP - Viralrecon

qc_parsed_linajes <- read_excel(dir_excel[1], sheet = 9)
df_parsed_linajes <- as.data.frame(qc_parsed_linajes)

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

matrix_tasa<- matrix(0, ncol = 10, nrow = 24)
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

matrix_tasa<- matrix(0, ncol = 10, nrow = 40)
for (j in 1:ncol(df_linajes_lab)) {
    control<- df_linajes_control[j]
    muestra<- df_linajes_lab[, j]
        for (i in 1:length(muestra)) {
            if (control == muestra[i]) {
                matrix_tasa [i, j]<- "Bien"
            } else if (muestra[i] == "None") {
                matrix_tasa [i, j]<- "Mal"
            } else if (control != muestra[i]) {
                matrix_tasa [i, j]<- "Mal"
            } 
        }   
}


matrix_valores_0 <- matrix(0, ncol = 1, nrow = 40)
for (i in 1:40) {
    tabla_0 <- table(matrix_tasa[i, ])
    matrix_valores_0[i, 1] <- as.numeric(tabla_0[2])
}

matrix_valores_0[7,]<- 0
matrix_valores_0[30,]<- 0
matrix_valores_0[32,]<- 0
matrix_valores_0[36,]<- 0
matrix_valores_0[37,]<- 0

df_aciertos<- data.frame(
    id = df_parsed_linajes$grupo[-1],
    aciertos = matrix_valores_0[,1]
)



#### Datos aciertos

#sort_df_aciertos_lin<- df_aciertos_lin[order(df_aciertos_lin$id), ]
#write.table(sort_df_aciertos_lin, "aciertos_linajes.csv", row.names = F, quote = F, sep = "\t")
#sort_df_aciertos_variantes<- df_aciertos_variantes[order(df_aciertos_variantes$id), ]
#write.table(sort_df_aciertos_variantes, "aciertos_variantes.csv", row.names = F, quote = F, sep = "\t")

qc_aciertos <- read_excel(dir_excel[1], sheet = 15)
niveles_aciertos<- c("HU Virgen del Rocio",
"HU San Cecilio",
"HU Miguel Servet",
"CIB de Aragón",
"HCU Lozano Blesa",
"HUC de Asturias",
"HU Son Espases",
"HU de Gran Canaria Dr. Negrín",
"HU Ntra. Sra de Candelaria",
"HU Marqués de Valdecilla",
"HU de Toledo",
"CHU de Albacete",
"HU de Ciudad Real",
"Consorcio LUCIA",
"HU Germans Trias i Pujol - 1",
"HU Germans Trias i Pujol - 2",
"H Dr. Josep Trueta",
"LR de Catalunya",
"HU Vall d’Hebron",
"H Clínic de Barcelona",
"Hospital Universitari Bellvitge - 1",
"Hospital Universitari Bellvitge - 2",
"Banc de Sang i Teixits Catalunya",
"HU de Badajoz",
"CHUVI",
"CHUS",
"CHUAC",
"Fundacion RiojaSalud",
"HU 12 DE OCTUBRE",
"HU La Paz",
"HU RAMÓN Y CAJAL",
"HU GREGORIO MARAÑÓN",
"HU Virgen de la Arrixaca",
"CH de Navarra - 1",
"CH de Navarra - 2",
"HU de Donostia",
"FISABIO - 1",
"FISABIO - 2",
"HGU de Alicante",
"HGU de Valencia",
"HGU de Elche")

niveles_id_aciertos<-c("COD_2109",
"COD_2129",
"COD_2122",
"COD_2126",
"COD_2132",
"COD_2143",
"COD_2110",
"COD_2102",
"COD_2111",
"COD_2104",
"COD_2101",
"COD_2127",
"COD_2136",
"COD_2131",
"COD_2107",
"COD_2107_2",
"COD_2114",
"COD_2115",
"COD_2121",
"COD_2123",
"COD_2124",
"COD_2124_2",
"COD_2141",
"COD_2119",
"COD_2112",
"COD_2135",
"COD_2137",
"COD_2113",
"COD_2103",
"COD_2105",
"COD_2116",
"COD_2125",
"COD_2120",
"COD_2117",
"COD_2117_2",
"COD_2108",
"COD_2106",
"COD_2106_2",
"COD_2134",
"COD_2139",
"COD_2140")

qc_aciertos$nombre<- factor(qc_aciertos$nombre, levels = niveles_aciertos)
qc_aciertos$id<- factor(qc_aciertos$id, levels = niveles_id_aciertos)
qc_aciertos$clase<- revalue(qc_aciertos$clase, c("linaje" = "Linajes", "variante" = "Variantes"))

rep_level<- c(
"COD_2106_2",
"COD_2117_2",
"COD_2124_2",
"COD_2107_2"
)

qc_aciertos_mod <- qc_aciertos[ !qc_aciertos$id %in% rep_level, ] 

ggplot(qc_aciertos_mod, aes(x = id, y = aciertos, fill = comunidad)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_grid(~clase) +
    guides(color = guide_legend(title = "Curves"), fill = guide_legend(title = "Comunidades")) +
    labs(x = "", y = "", title = "") +
    scale_y_discrete(name ="Aciertos", 
                    limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
    theme(
    text = element_text(size = 18),
    axis.text.y = element_text())
    axis.text.x = element_text())
ggsave("Graficos/qc_aciertos.png")

# Sensibilidad y precision

levels_samples_lin<- c("Muestra 1 B.1.1.7",
"Muestra 2 B.1.351",
"Muestra 3 A.28",
"Muestra 4 B.1.621",
"Muestra 5 P.1",
"Muestra 6 AY.9.2",
"Muestra 7 AY.43",
"Muestra 8 AY.94",
"Muestra 9 AY.94",
"Muestra 10 AY.43")

levels_samples_var<- c("Muestra 1 Alfa",
"Muestra 2 Beta",
"Muestra 3 None",
"Muestra 4 Mu",
"Muestra 5 Gamma",
"Muestra 6 Delta",
"Muestra 7 Delta",
"Muestra 8 Delta",
"Muestra 9 Delta",
"Muestra 10 Delta")

qc_tasa <- read_excel(dir_excel[1], sheet = 13)

qc_tasa_linajes<- subset(qc_tasa, clase == "linajes")
qc_tasa_linajes$samples<- factor (qc_tasa_linajes$samples, levels = levels_samples_lin)
qc_tasa_linajes$tipo<- factor (qc_tasa_linajes$tipo, levels = c("Sensitivity", "Precision"))

qc_tasa_variantes<- subset(qc_tasa, clase == "variantes")
qc_tasa_variantes$samples<- factor (qc_tasa_variantes$samples, levels = levels_samples_var)
qc_tasa_variantes$tipo<- factor (qc_tasa_variantes$tipo, levels = c("Sensitivity", "Precision"))

# mutaciones por muestra

matrix_mutaciones <- matrix(0, ncol = 2, nrow = 10)
for (i in 1:10) {
    matrix_mutaciones[i, 1]<- mean(variants_data$variants[variants_data$muestra == unique(variants_data$muestra)[i]], na.rm = T)
    matrix_mutaciones[i, 2]<- median(variants_data$variants[variants_data$muestra == unique(variants_data$muestra)[i]], na.rm = T)
}

qc_tasa_linajes$mutaciones<- round(as.numeric(rep (matrix_mutaciones[,1], 2)))
qc_tasa_variantes$mutaciones<- round(as.numeric(rep (matrix_mutaciones[,1], 2)))

qc_tasa_linajes$tipo<- revalue( qc_tasa_linajes$tipo, c("Sensitivity" = "Sensibilidad", "Precision" = "Precisión"))
qc_tasa_variantes$tipo<- revalue( qc_tasa_variantes$tipo, c("Sensitivity" = "Sensibilidad", "Precision" = "Precisión"))

# plot sensitivity, precision & mutations

resultado_data<- rbind(qc_tasa_linajes, qc_tasa_variantes)
prueba_nombres<- rep(nombres_muestras, 4)
resultado_data$samples<- factor(prueba_nombres, levels = nombres_muestras)
resultado_data$clase<- revalue(resultado_data$clase, c("linajes" = "Linajes", "variantes" = "Variantes"))

ggplot(resultado_data, aes(x = samples, y = tasa * 100, group = tipo)) + 
    geom_point(aes(x = samples, y = mutaciones), size = 3) +
    geom_line(aes(color = tipo), size = 2) +
    facet_grid(tipo~clase) +
        scale_y_continuous(breaks = c(0,20,40,60,80,100), limits = c(0,100),
        name = "%",
        sec.axis = sec_axis(trans=~./1.05, name="Mutaciones")
    ) +
    guides(color = guide_legend(title = "Curvas"), fill = guide_legend(title = "")) +
    labs(x = "", y = "%", title = "") +
    theme(
    text = element_text(size = 20),
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45))

ggsave("Graficos/qc_sensitivity_precision_mutaciones.png")
