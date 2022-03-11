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

### datos Resultados ----

qc_resultados <- read_excel(dir_excel[1], sheet = 3)

### datos Fechas ----

qc_fechas <- read_excel(dir_excel[1], sheet = 2)

# interesante las fechas de envio, recepcion y emision

fechas_data<- data.frame (
  id = as.character(qc_fechas$ID),
  envio = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de envio`)), date_system = "modern"),
  recepcion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de recepcion`)), date_system = "modern"),
  secuenciacion = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de secuenciacion`)), date_system = "modern"),
  emision = excel_numeric_to_date(as.numeric(as.character(qc_fechas$`Fecha de emision de resultados`)), date_system = "modern"),
  ejecucion = as.numeric(qc_fechas$`Tiempo de ejecucion`)
)

#### plot tiempo de ejecucion ----
# tabla con estadisticas
# indicar que los lab que han fallado
ggplot (fechas_data, aes(x = recepcion, xend = secuenciacion,
                         y = id, yend = id, color = "#4a8abe")) + 
  geom_segment(size = 3, show.legend = F) +
  geom_text(aes(label=fechas_data$ejecucion), position = position_dodge(width = 1), hjust=-1, color="black", size=2.5) +
  labs(x = "Execution time (days)", y = "")
ggsave("Graficos/qc_tiempo_ejecucion.png")

ggplot(fechas_data, aes(y = ejecucion)) +
  geom_boxplot() +
  labs(x = "", y = "Execution time (days)",  title = "")
  ggsave("Graficos/qc_tiempo_ejecucion_estadistica.png")

#### Datos estadistica -----

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
  variants_effect = as.numeric(qc_estadistica$var_variantseffect),
  protocolo = as.character(qc_resultados)
)

##### Plot ct -----

ggplot(estadistica_data, aes(x = muestra, y = ct, fill = "steelblue")) +
  geom_boxplot(show.legend = F) +
  facet_grid(~)
  labs(x = "", y = "Ct",  title = "Valores de Ct") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#### datos reads -----
lista_read<- list ()
for (i in 1:nrow(estadistica_data)){
  
  read_t<- t(estadistica_data[i,c(7,8,12)])
  lista_read[[i]]<- data.frame(id = i, read = read_t[,1], tipo = c("read_n", "quality", "quality_10"))
  
}

df_read <- bind_rows(lista_read, .id = "id")

lista_nombres<- list()
for (i in 1:40){
  
  n_nombres_labs<- rep (nombres_labs[i], 40)
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

#### datos estadistica Ct ----

nombres_labs<- unique(estadistica_data$id)

lista_nombres<- list()
for (i in 1:40){
  
  n_nombres_labs<- rep (nombres_labs[i], 40)
  lista_nombres[[i]]<- as.character(n_nombres_labs)
  
}

id_lab<- unlist(lista_nombres)

lista_ct<- list ()
for (i in 1:nrow(estadistica_data)){
  
  ct_t<- t(estadistica_data[i,3:6])
  lista_ct[[i]]<- data.frame(id = i, Ct = ct_t[,1], tipo = c("PCR", "N", "ORF", "S"))
  
}

df_ct <- bind_rows(lista_ct, .id = "id")

ct_data<- data.frame (id = id_lab,
df_ct[,c(2,3)])

row.names(ct_data)<- NULL
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
