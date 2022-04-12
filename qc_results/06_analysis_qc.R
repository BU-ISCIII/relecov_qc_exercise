
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
    labs(x = "", y = "", title = "") +
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
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Plataforma")) +
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
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Plataforma")) +
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

ggplot(subset(variants_data, plataforma == "Illumina"), aes(x = software, y = variants)) +
    geom_bar(aes(), stat = "identity", position = position_dodge()) +
    geom_boxplot(fill = "#1F77B4", outlier.colour="red") +
    facet_grid(~tipo) +
    scale_y_continuous(
        name = "Mutaciones",
        sec.axis = sec_axis(trans=~./24, name="Software")
    ) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Samples")) +
    labs(x = "", y = "Mutaciones / software", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    text = element_text(size = 25))
ggsave("Graficos/qc_variantes_software_illumina.png", dpi = 500, units = "cm", width = 25, height = 25)

#### plot variant callers ----

aa<- subset(bioinfo_data, platform == "Illumina")
table(aa$variant)
ggplot(bioinfo_data, aes(variant, fill = platform)) +
    geom_bar() +
    guides(fill = guide_legend(title = "Plataforma")) +
    labs(y = "Número de laboratorios", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count))) +
    theme(
    text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p1 + p2
ggsave("Graficos/qc_variantes_software.png", dpi = 500, units = "cm", width = 25, height = 25)
