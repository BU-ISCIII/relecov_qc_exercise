
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

### Excel ----

dir_excel <- list.files(path = "Data", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

# levels COD_lab

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

Sys.setlocale("LC_TIME", "English")
format(Sys.Date(), format = "%Y-%b-%d")
ggplot(fechas_data, aes(
    x = as.Date(fechas_data$recepcion), xend = as.Date(fechas_data$secuenciacion),
    y = id, yend = id, color = "#4a8abe"
)) +
    geom_segment(size = 3, show.legend = F) +
    geom_text(aes(label = fechas_data$ejecucion), position = position_dodge(width = 1), hjust = 1.2, color = "black", size = 5) +
    labs(x = "Tiempo de ejecución (dias)", y = "") +
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
nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")

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

ggplot(virushost_data, aes(x = muestra2, y = porcentaje)) +
    geom_boxplot(fill = "#1F77B4") +
    facet_grid(plataforma ~ tipo) +
    labs(x = "", y = "", title = "") +
    theme(
        text = element_text(size = 22),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45)
    )
ggsave("Graficos/qc_resultados_hostvirusunmapped_plataforma.png")

#### Datos estadistica -----

qc_depth <- read_excel(dir_excel[1], sheet = 6)
nombres_muestras <- c("sample_1", "sample_2", "sample_3", "sample_4", "sample_5", "sample_6", "sample_7", "sample_8", "sample_9", "sample_10")

depth_data <- data.frame(
    id = as.character(qc_depth$ID),
    muestra = as.character(qc_depth$`Sample ID`),
    muestra2 = factor(rep(nombres_muestras, 41), levels = nombres_muestras),
    plataforma = as.character(qc_depth$plataforma),
    depth = as.numeric(qc_depth$depth),
    tipo = factor(qc_depth$tipo, levels = c("mean_depth_coverage", "var_QC>10x"))
)

virushost_data$plataforma <- factor(virushost_data$plataforma, levels = c("Illumina", "Ion Torrent", "Nanopore"))
virushost_data$id <- factor(virushost_data$id, levels = levels_id)

##### Plot depth -----

ggplot(subset(depth_data, depth > 5 & tipo == "var_QC>10x"), aes(x = muestra2, y = log10(depth))) +
    geom_boxplot(fill = "#1F77B4") +
    facet_grid(~plataforma) +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    labs(x = "", y = "log10(coverage (mean depth)) / sample", title = "") +
    theme(
        text = element_text(size = 22),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45)
    )
ggsave("Graficos/qc_resultados_coverage_platform_samples.png")

ggplot(subset(estadistica_data, mean_depth > 5), aes(log10(mean_depth))) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(fill = "#FF6666", position = "identity", alpha = 0.6) +
    guides(fill = guide_legend(title = "Platform")) +
    labs(x = "log10 (mean depth)", y = "Density", title = "Sample 7 - AY.43") +
    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_text(size = 12), legend.text = element_text(size = 12), strip.text.x = element_text(size = 12))

ggplot(subset(estadistica_data, mean_depth > 5), aes(x = muestra2, y = log10(mean_depth), fill = plataforma)) +
    geom_boxplot() +
    guides(color = guide_legend(title = "Samples"), fill = guide_legend(title = "Platform")) +
    labs(x = "", y = "log10(coverage (mean depth)) / sample", title = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))
ggsave("Graficos/qc_resultados_coverage_platform_samples.png")

ggplot(subset(estadistica_data, mean_depth > 5 & muestra2 == "sample_7"), aes(log10(mean_depth))) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(fill = "#FF6666", position = "identity", alpha = 0.6) +
    guides(fill = guide_legend(title = "Platform")) +
    labs(x = "log10 (mean depth)", y = "Density", title = "Sample 7 - AY.43") +
    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_text(size = 12), legend.text = element_text(size = 12), strip.text.x = element_text(size = 12))
ggsave("Graficos/qc_resultados_coverage_platform_sample7.png")