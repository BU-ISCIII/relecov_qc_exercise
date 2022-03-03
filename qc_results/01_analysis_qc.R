#!/usr/bin/env Rscript

# library -------

library (plyr, quietly = TRUE, warn.conflicts = FALSE)
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

dir_excel<-list.files(path = "Data", pattern = "xlsx", full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

### datos Fechas ----

qc_fechas <- read_excel(dir_excel[1], sheet = 2)

# interesante las fechas de envio, recepcion y emision

fechas_data<- data.frame (
  id = as.character(qc_fechas$ID),
  envio = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de envio`)), date_system = "modern"),
  recepcion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de recepcion`)), date_system = "modern"),
  secuenciacion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de secuenciacion`)), date_system = "modern"),
  emision = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de emision de resultados`)), date_system = "modern"),
  ejecucion = qc_fechas$`Tiempo de ejecucion`
)


#### plot tiempo de ejecucion ----
ggplot (fechas_data, aes(x = recepcion, xend = secuenciacion, 
                         y = id, yend = id, color = id)) + 
  geom_segment(size = 3, show.legend = F) +
  labs(x = "", y = "", title = "Tiempo de ejecucion")
ggsave("Graficos/qc_tiempo_ejecucion.png")

#### plot Envio ----
getwd()
ggplot (fechas_data, aes(y = id, x = as.character(envio))) +
  geom_point() +
  labs(x = "", y = "", title = "Send samples dates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_fechas_envio.png")

#### plot Recepcion ----
ggplot (fechas_data, aes(y = id, x = as.character(recepcion))) +
  geom_point() +
  labs(x = "", y = "", title = "Reception samples dates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_fechas_recepcion.png")

#### plot Secuencing ----
ggplot (fechas_data, aes(y = id, x = as.character(secuenciacion))) +
  geom_point() +
  labs(x = "", y = "", title = "Sequencing samples dates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_fechas_secuenciacion.png")

#### plot Emision ----
ggplot (fechas_data, aes(y = id, x = as.character(emision))) +
  geom_point() +
  labs(x = "", y = "", title = "Emission samples dates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_fechas_emision.png")

### datos Fechas totales ----

qc_fechas <- read_excel(dir_excel[1], sheet = 3)

fechas_data<- data.frame (
  id = as.character(qc_fechas$ID),
  tipo = as.character(qc_fechas$tipo),
  fecha = excel_numeric_to_date(as.numeric(as.character(qc_fechas$fecha)), date_system = "modern")
)

levels(fechas_data$tipo)[match("Sequencing date",levels(fechas_data$tipo))] <- "Fecha de secuenciacion"

#### plot fechas totales -----

ggplot (fechas_data, aes(y = id, x = as.character(fecha), color = tipo)) +
  geom_point() +
  labs(x = "", y = "", title = "Fechas de envio, recepcion, emision y secuenciacion del QC") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6))
ggsave("Graficos/qc_fechas_total.png")

### datos Resultados ----

qc_resultados <- read_excel(dir_excel[1], sheet = 4)

#### Protocolo de diagnostico -----

protocolo_data<- data.frame (
  id = as.character(qc_resultados$ID),
  protocolo = as.character(qc_resultados$`Protocolo diagnostico SARS-CoV-2`)
)

n_protocolo_data<- protocolo_data[!duplicated(protocolo_data$id), ]

##### Plot Protocolo de diagnostico -----

ggplot (n_protocolo_data, aes(y = id, x = as.character(protocolo))) +
  geom_point() +
  labs(x = "", y = "",  title = "Protocolo diagnostico SARS-CoV-2 ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.y = element_text(size = 6))
ggsave("Graficos/qc_resultado_protocolo.png")

#### Genoma de referencia -----

genoma_data<- data.frame (
  id = as.character(qc_resultados$ID),
  muestra = as.character(qc_resultados$`Sample ID`),
  genoma = as.character(qc_resultados$var_referencegenomeaccession)
)

n_2_genoma_data <- genoma_data %>% 
  group_by(genoma) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round (count/sum(count), 3))

##### Plot Genoma de referencia -----

ggplot(n_2_genoma_data, aes(x = factor(genoma), y = perc*100)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "", y = "",  title = "Genoma de referencia") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_resultado_genoma.png")

### datos Secuenciacion ----

qc_secuenciacion <- read_excel(dir_excel[1], sheet = 5)

#### Plataformas de secuenciacion -----

plataforma_data<- data.frame (
  id = as.character(qc_secuenciacion$ID),
  muestra = as.character(qc_secuenciacion$`Sample ID`),
  plataforma = as.character(qc_secuenciacion$`Sequencing platforms (Illumina, Nanopore, IonTorrent, PacBio, other)`)
)

n_plataforma_data<- plataforma_data[!duplicated(plataforma_data$id), ]

##### Plot Plataforma secuenciacion -----

ggplot (n_plataforma_data, aes(y = id, x = as.character(plataforma))) +
  geom_point() +
  labs(x = "", y = "",  title = "Plataforma de secuenciacion") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.y = element_text(size = 6))
ggsave("Graficos/qc_secuenciacion_plataforma.png")

#### Enriquecimiento -----

qc_enriquecimiento <- read_excel(dir_excel[1], sheet = 5)

qc_enriquecimiento$variable_amplicon[qc_enriquecimiento$variable_amplicon == "NA"]<- NA

enriquecimiento_data<- data.frame (
  id = as.character(qc_enriquecimiento$ID),
  muestra = as.character(qc_enriquecimiento$`Sample ID`),
  plataforma = as.character(qc_enriquecimiento$sequencing_platforms_mod),
  enriquecimiento = as.character(qc_enriquecimiento$variable_amplicon)
)

n_enriquecimiento_data<- enriquecimiento_data[!duplicated(enriquecimiento_data$id), ]

##### Plot Enriquecimiento -----

ggplot (n_enriquecimiento_data, aes(y = id, x = as.character(enriquecimiento))) +
  geom_point(aes (color = plataforma)) +
  labs(x = "", y = "",  title = "Protocolo de enriquecimiento y versiones") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.y = element_text(size = 6))
ggsave("Graficos/qc_secuenciacion_enriquecimiento.png")

#### Librerias -----

qc_librerias <- read_excel(dir_excel[1], sheet = 5)

librerias_data<- data.frame (
  id = as.character(qc_librerias$ID),
  muestra = as.character(qc_librerias$`Sample ID`),
  librerias = as.character(qc_librerias$library_variable),
  layout = as.character(qc_librerias$sequencing_platforms_mod),
  plataforma = as.character(qc_librerias$sequencing_platforms_mod)
)

n_librerias_data<- librerias_data[!duplicated(librerias_data$id), ]

##### Plot librerias -----

ggplot (n_librerias_data, aes(y = id, x = as.character(librerias))) +
  geom_point(aes (color = plataforma)) +
  labs(x = "", y = "",  title = "Librerias y kit") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.y = element_text(size = 6))
ggsave("Graficos/qc_secuenciacion_librerias.png")

#### Muestras por carrera -----

qc_carreras <- read_excel(dir_excel[1], sheet = 5)

carreras_data<- data.frame (
  id = as.character(qc_carreras$ID),
  muestra = as.character(qc_carreras$`Sample ID`),
  carreras = as.numeric(as.vector(qc_carreras$`Number samples in run`)),
  read = as.numeric(as.vector(qc_carreras$`Read lenght`)),
  layout = as.character(qc_carreras$`Library layout`),
  phi = as.character(qc_carreras$`Was PhiX used in the sequencing?`),
  plataforma = as.character(qc_librerias$sequencing_platforms_mod)
)

##### Plot Muestras por carrera -----

n_carreras_data<- carreras_data[!duplicated(carreras_data$id), ]

ggplot(n_carreras_data, aes(x=id, y=carreras, fill=plataforma)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=carreras), vjust=1.6, color="black", size=3) +
  labs(x = "", y = "muestras/carreras",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 6),
        axis.text.y = element_text(size = 7))
ggsave("Graficos/qc_secuenciacion_carreras.png")

##### Plot read length -----

ggplot(carreras_data, aes(x=id, y=read/10, fill = plataforma)) +
  geom_col() +
  labs(x = "", y = "longitud read / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8),
        axis.text.y = element_text(size = 7))
ggsave("Graficos/qc_secuenciacion_reads.png")

##### Plot phiX -----

ggplot(n_carreras_data, aes(phi, id, color = plataforma)) +
  geom_count (show.legend = T) +
  labs(x = "", y = "", title = "Use PhiX by platform")
ggsave("Graficos/qc_secuenciacion_phiX.png")

### datos Bioinformatica ----

qc_bioinformatica<- read_excel(dir_excel[1], sheet = 7)

progbio<- data.frame (
  id = as.character(qc_bioinformatica$ID),
  protocolo = as.character(qc_bioinformatica$var_bioinformatic_protocol),
  version = as.character(qc_bioinformatica$var_version_bioinfo_protocol),
  protocolo_version = as.character(qc_bioinformatica$variable_unite_bioinfo_version)
)

n_progbio_data <- progbio %>% 
  group_by(protocolo_version) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round (count/sum(count), 3))

##### Plot protocolo de bioinformatica y version -----

ggplot(n_progbio_data, aes(x = factor(protocolo_version), y = perc*100)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "", y = "",  title = "Protocolo bioinformatico") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinformatica_protocolo.png")

### datos estadistica ----

qc_estadistica<- read_excel(dir_excel[1], sheet = 4)

estadistica_data<- data.frame (
  id = as.character(qc_estadistica$ID),
  muestra = as.character(qc_estadistica$`Sample ID`),
  ct = as.numeric(qc_estadistica$`Valor Ct PCR`),
  ct_N = as.numeric(qc_estadistica$`Valor Ct N`),
  ct_ORF = as.numeric(qc_estadistica$`Valor Ct ORF`),
  ct_S = as.numeric(qc_estadistica$`Valor Ct S`),
  readc = log10(as.numeric(qc_estadistica$var_readcount)),
  qc_filtered = as.numeric(qc_estadistica$var_qcfiltered),
  host = as.numeric(qc_estadistica$var_readhost),
  virus = as.numeric(qc_estadistica$var_readsvirus),
  unmapped = as.numeric(qc_estadistica$var_unmapped),
  qc10x = as.numeric(qc_estadistica$`var_QC>10x`),
  mean_depth = as.numeric(qc_estadistica$var_mean_depth_coverage),
  perc_Ns = as.numeric(qc_estadistica$var_n_Ns),
  variants_75 = as.numeric(qc_estadistica$var_number_variants_75),
  variants_effect = as.numeric(qc_estadistica$var_variantseffect)
)

#### datos unmapped virus, % genoma 10x y media de la profundidad -----

unmapped_data<- data.frame(
  id = as.character(estadistica_data$id),
  muestra = factor(estadistica_data$muestra, levels = c("#1","#2","#3","#4","#5","#6","#7","#8","#9","#10")),
  host = as.numeric(estadistica_data$host),
  virus = as.numeric(estadistica_data$virus),
  unmapped = as.numeric(estadistica_data$unmapped),
  q10x =  as.numeric(estadistica_data$qc10x),
  depth = as.numeric(estadistica_data$mean_depth)
)

##### Plot host -----

ggplot(unmapped_data, aes(x = id, y = host)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "% mapped reads (host) / sample",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7))
ggsave("Graficos/qc_resultados_read_host.png")

##### Plot virus -----

ggplot(unmapped_data, aes(x = id, y = virus)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "% mapped reads (virus) / sample",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7))
ggsave("Graficos/qc_resultados_read_virus.png")

##### Plot unmapped virus -----

ggplot(unmapped_data, aes(x = id, y = unmapped)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "% ummapped reads (virus) / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7))
ggsave("Graficos/qc_resultados_unmapped_virus.png")

##### Plot % genoma 10x -----

ggplot(unmapped_data, aes(x = id, y = q10x)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "% genoma 10x / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7))
ggsave("Graficos/qc_resultados_genoma_10x.png")

##### Plot depth -----

ggplot(unmapped_data, aes(x = id, y = log10(depth))) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "log(Cobertura (media de la profundiad)) / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8))
ggsave("Graficos/qc_resultados_coverage.png")

#### datos %N -----

ns_data<- data.frame(
  id = as.character(estadistica_data$id),
  muestra = factor(estadistica_data$muestra, levels = c("#1","#2","#3","#4","#5","#6","#7","#8","#9","#10")),
  Ns = as.numeric(estadistica_data$perc_Ns)
)

##### Plot Ns -----

ggplot(ns_data, aes(x = id, y = Ns, fill = muestra)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x = "", y = "Porcentaje de Ns / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8))
ggsave("Graficos/qc_resultados_Ns.png")

#### datos variantes -----

variantes_data<- data.frame(
  id = as.character(estadistica_data$id),
  muestra = factor(estadistica_data$muestra, levels = c("#1","#2","#3","#4","#5","#6","#7","#8","#9","#10")),
  variants_75 = as.numeric(qc_estadistica$var_number_variants_75),
  variants_effect = as.numeric(qc_estadistica$var_variantseffect),
  ct = estadistica_data$ct
)

##### Plot variantes -----

ggplot(variantes_data, aes(x = id, y = variants_75)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "Numero de variantes (AF > 0.75) / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8))
ggsave("Graficos/qc_resultados_variantes_AF75.png")

ggplot(variantes_data, aes(x = id, y = variants_effect)) +
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2), aes (color = muestra)) +
  labs(x = "", y = "Numero de variantes con efecto / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 8))
ggsave("Graficos/qc_resultados_variantes_efecto.png")

#### datos reads -----
lista_read<- list ()
for (i in 1:nrow(estadistica_data)){
  
  read_t<- t(estadistica_data[i,c(7,8,12)])
  lista_read[[i]]<- data.frame(id = i, read = read_t[,1], tipo = c("read_n", "quality", "quality_10"))
  
}

df_read <- bind_rows(lista_read, .id = "id")

lista_nombres<- list()
for (i in 1:40){
  
  n_nombres_labs<- rep (nombres_labs[i], 30)
  lista_nombres[[i]]<- as.character(n_nombres_labs)
  
}

id_lab<- unlist(lista_nombres)

read_data<- data.frame (id = id_lab,
                      df_read[,c(2,3)]); row.names(read_data)<- NULL
read_data$tipo<- factor(read_data$tipo, levels = c("read_n", "quality", "quality_10"))

##### Plot reads -----
plot_data_read<- read_data[read_data$tipo == "read_n" | read_data$tipo == "quality_10", ]

revalue(plot_data_read$tipo, c("read_n" = "Reads", 
                          "quality_10" = "Porcentaje reads que han pasado el QC (en base 10)"
)) -> plot_data_read$tipo

ggplot(plot_data_read, aes(x = id, y = read, fill = tipo)) +
  geom_boxplot(show.legend = F) +
  facet_grid(~tipo) +
  labs(x = "", y = "log(reads) / muestra",  title = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7))
ggsave("Graficos/qc_resultados_read_qc.png")

#### datos estadistica Ct ----

lista_ct<- list ()
for (i in 1:nrow(estadistica_data)){
  
  ct_t<- t(estadistica_data[i,3:6])
  lista_ct[[i]]<- data.frame(id = i, Ct = ct_t[,1], tipo = c("PCR", "N", "ORF", "S"))
  
}

df_ct <- bind_rows(lista_ct, .id = "id")

nombres_labs<- unique(estadistica_data$id)
lista_nombres<- list()
for (i in 1:40){
  
  n_nombres_labs<- rep (nombres_labs[i], 40)
  lista_nombres[[i]]<- as.character(n_nombres_labs)
  
}

ct_data<- data.frame (id = id_lab,
  df_ct[,c(2,3)]); row.names(ct_data)<- NULL
ct_data$tipo<- factor(ct_data$tipo, levels = c("PCR", "ORF", "S", "N"))

##### Plot ct -----

ggplot(ct_data, aes(x = id, y = Ct, fill = "steelblue")) +
  geom_boxplot(show.legend = F) +
  labs(x = "", y = "Ct",  title = "Valores de Ct") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Graficos/qc_resultados_ct.png")

##### Plot ct genes -----

ggplot(ct_data, aes(x = id, y = Ct, fill = tipo)) +
  geom_boxplot() +
  labs(x = "", y = "Ct",  title = "Valores de Ct") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Graficos/qc_resultados_ct_genes.png")
