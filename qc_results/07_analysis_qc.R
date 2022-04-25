
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
library(reshape2, quietly = TRUE, warn.conflicts = FALSE)

### Excel ----

dir_excel <- list.files(path = "Data/Pangolin", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos Fechas ----

qc_pangolin <- read_excel(dir_excel[1], sheet = 1)

nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")
# nombres_muestras$tipo <- revalue(virushost_data$tipo, c("host" = "% de huésped", "virus" = "% de virus", "unmapped" = "% de lecturas no mapeadas"))

pangolin_data <- data.frame(
    grupo = as.character(qc_pangolin$group),
    muestra = as.character(qc_pangolin$sample),
    linajes = as.character(qc_pangolin$lineage),
    version = as.character(qc_pangolin$version)
)

pangolin_data$muestra <- factor(pangolin_data$muestra, levels = nombres_muestras)

# plots

# v3.1.16

ggplot(subset(pangolin_data, version == "v3.1.16"), aes(muestra, fill = linajes)) +
    geom_bar() +
    guides(fill = guide_legend(title = "")) +
    labs(y = "v3.1.16", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 12) +
    theme(
        text = element_text(size = 55),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_16.png", width = 75, height = 65, dpi = 300, units = c("cm"))


# todas

ggplot(pangolin_data, aes(muestra, fill = linajes)) +
    geom_bar() +
    guides(fill = guide_legend(title = "")) +
    facet_grid(~version) +
    labs(y = "", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 12) +
    theme(
        text = element_text(size = 55),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin.png", width = 75, height = 65, dpi = 300, units = c("cm"))

# todas

ggplot(subset(pangolin_data, ), aes(muestra, fill = linajes)) +
    geom_bar() +
    guides(fill = guide_legend(title = "")) +
    facet_grid(~version) +
    labs(y = "", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 12) +
    theme(
        text = element_text(size = 55),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin.png", width = 75, height = 65, dpi = 300, units = c("cm"))