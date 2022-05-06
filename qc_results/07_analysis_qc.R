
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
library(ggimage, quietly = TRUE, warn.conflicts = FALSE)

### Excel ----

dir_excel <- list.files(path = "Data/Pangolin", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos laboratorios ----

qc_pangolin <- read_excel(dir_excel[1], sheet = 1)

nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")
# nombres_muestras$tipo <- revalue(virushost_data$tipo, c("host" = "% de huésped", "virus" = "% de virus", "unmapped" = "% de lecturas no mapeadas"))

pangolin_data_lab <- data.frame(
    grupo = as.character(qc_pangolin$group),
    muestra = as.character(qc_pangolin$sample),
    linajes = as.character(qc_pangolin$lineage),
    version = as.character(qc_pangolin$version)
)

pangolin_data_lab$muestra <- factor(pangolin_data_lab$muestra, levels = nombres_muestras)
# pangolin_data_lab$programa <- as.character(rep("Laboratorios", length(pangolin_data_lab$grupo)))

# wide table

wide_lab <- pangolin_data_lab %>%
    spread(version, linajes)
write.table(wide_lab, "df_pangolin_lab_wide.csv", row.names = F, sep = "\t", quote = F)


# plots

ggplot(pangolin_data_lab, aes(muestra, fill = linajes)) +
    geom_bar() +
    facet_grid(~version) +
    guides(fill = guide_legend(title = "")) +
    labs(y = "", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 10) +
    theme(
        text = element_text(size = 45),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_lab.png", width = 75, height = 55, dpi = 300, units = c("cm"))

### datos viralrecon ----

qc_pangolin <- read_excel(dir_excel[1], sheet = 2)

nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")

pangolin_data_viralrecon <- data.frame(
    grupo = as.character(qc_pangolin$group),
    muestra = as.character(qc_pangolin$sample),
    linajes = as.character(qc_pangolin$lineage),
    version = as.character(qc_pangolin$version)
)

pangolin_data_viralrecon$muestra <- factor(pangolin_data_viralrecon$muestra, levels = nombres_muestras)
# pangolin_data_viralrecon$programa <- as.character(rep("Viralrecon", length(pangolin_data_viralrecon$grupo)))

# wide table

wide_viralrecon <- pangolin_data_viralrecon %>%
    spread(version, linajes)
write.table(wide_viralrecon, "df_pangolin_viralrecon_wide.csv", row.names = F, sep = "\t", quote = F)

# plots

ggplot(pangolin_data_viralrecon, aes(muestra, fill = linajes)) +
    geom_bar() +
    facet_grid(~version) +
    guides(fill = guide_legend(title = "")) +
    facet_grid(~version) +
    labs(y = "", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 12) +
    theme(
        text = element_text(size = 55),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_viralrecon.png", width = 75, height = 65, dpi = 300, units = c("cm"))

### datos control ----

qc_pangolin <- read_excel(dir_excel[1], sheet = 4)

nombres_muestras <- c("muestra 1", "muestra 2", "muestra 3", "muestra 4", "muestra 5", "muestra 6", "muestra 7", "muestra 8", "muestra 9", "muestra 10")

pangolin_data_control <- data.frame(
    grupo = as.character(qc_pangolin$group),
    muestra = as.character(qc_pangolin$sample),
    linajes = as.character(qc_pangolin$lineage),
    version = as.character(qc_pangolin$version)
)

pangolin_data_control$muestra <- factor(pangolin_data_control$muestra, levels = nombres_muestras)
pangolin_data_control$programa <- as.character(rep("Control", length(pangolin_data_control$grupo)))

# plots

ggplot(pangolin_data_control, aes(muestra, fill = linajes)) +
    geom_bar() +
    facet_grid(~version) +
    guides(fill = guide_legend(title = "")) +
    facet_grid(~version) +
    labs(y = "", x = "", title = "") +
    geom_text(stat = "count", position = position_stack(vjust = 0.5), aes(label = after_stat(count)), size = 12) +
    theme(
        text = element_text(size = 55),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_control.png", width = 75, height = 65, dpi = 300, units = c("cm"))

# Analisis en conjunto wide

wide_lab$programa <- as.character(rep("Laboratorios", length(wide_lab$grupo)))
wide_viralrecon$programa <- as.character(rep("Viralrecon", length(wide_viralrecon$grupo)))

head(wide_viralrecon)

data_conjunto <- merge(wide_lab, wide_viralrecon, by = c("grupo", "muestra"), all = TRUE)

str(data_conjunto)

# write.table(data_conjunto, "df_pangolin_conjunto_wide_2.csv", row.names = F, sep = "\t", quote = F)

# Analisis en conjunto

grupos_eliminar <- c(
    "COD_2101",
    "COD_2102",
    "COD_2104",
    "COD_2105",
    "COD_2106_Nanopore",
    "COD_2107_MinION",
    "COD_2115",
    "COD_2119",
    "COD_2120",
    "COD_2127",
    "COD_2136",
    "COD_2140",
    "COD_2143"
)

`%notin%` <- Negate(`%in%`)

pangolin_data_lab$programa <- as.character(rep("Laboratorios", length(pangolin_data_lab$grupo)))
pangolin_data_viralrecon$programa <- as.character(rep("Viralrecon", length(pangolin_data_viralrecon$grupo)))

data_conjunto <- rbind(pangolin_data_lab, pangolin_data_viralrecon)

df_conjunto <- data_conjunto[data_conjunto$grupo %notin% grupos_eliminar, ]

# write.table(df_conjunto, "df_conjunto.csv", row.names = F, sep = "\t", quote = F)


# TODAS
ggplot(subset(df_conjunto, muestra == "muestra 1"), aes(x = grupo, y = linajes, color = version)) +
    geom_jitter() +
    facet_grid(~programa) +
    labs(y = "", x = "", title = "") +
    theme(
        text = element_text(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_conjunto_muestra1.png")

ggplot(subset(df_conjunto, programa == "Laboratorios"), aes(x = grupo, y = linajes, color = version)) +
    geom_jitter() +
    facet_grid(programa ~ muestra) +
    labs(y = "", x = "", title = "") +
    theme(
        text = element_text(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_conjunto_lab.png")

ggplot(subset(df_conjunto, programa == "Viralrecon"), aes(x = grupo, y = linajes, color = version)) +
    geom_jitter() +
    facet_grid(programa ~ muestra) +
    labs(y = "", x = "", title = "") +
    theme(
        text = element_text(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
ggsave("Graficos/qc_pangolin_conjunto_viralrecon.png")
