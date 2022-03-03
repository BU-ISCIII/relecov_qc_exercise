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

### datos principales ----

qc_analysis_bio <- read_excel(path = "qc_bioinformatic_analysis.xlsx", sheet = 2)

### datos Illumina iSeq ----

iseq_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "Illumina iSeq", ]

##### Plot Preprocessing -----

ggplot(iseq_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_iseq_pre.png")

##### Plot Mapping -----

ggplot(iseq_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_iseq_Mapping.png")

##### Plot Assembly -----

ggplot(iseq_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_iseq_Assembly.png")

##### Plot Variant calling -----

ggplot(iseq_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_iseq_vc.png")

##### Plot Consensus -----

ggplot(iseq_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_iseq_Consensus.png")

##### Plot linaje -----

ggplot(iseq_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_iseq_linage.png")

### datos Illumina miSeq ----

miseq_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "Illumina MiSeq", ]

##### Plot Preprocessing -----

ggplot(miseq_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_miseq_pre.png")

##### Plot Mapping -----

ggplot(miseq_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_miseq_Mapping.png")

##### Plot Assembly -----

ggplot(miseq_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_miseq_Assembly.png")

##### Plot Variant calling -----

ggplot(miseq_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_miseq_vc.png")

##### Plot Consensus -----

ggplot(miseq_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_miseq_Consensus.png")

##### Plot linaje -----

ggplot(miseq_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_miseq_linage.png")


### datos Illumina nextSeq ----

nextseq_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "Illumina NextSeq", ]

##### Plot Preprocessing -----

ggplot(nextseq_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_nextseq_pre.png")

##### Plot Mapping -----

ggplot(nextseq_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_nextseq_Mapping.png")

##### Plot Assembly -----

ggplot(nextseq_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_nextseq_Assembly.png")

##### Plot Variant calling -----

ggplot(nextseq_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_nextseq_vc.png")

##### Plot Consensus -----

ggplot(nextseq_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_nextseq_Consensus.png")

##### Plot linaje -----

ggplot(nextseq_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_nextseq_linage.png")


### datos Illumina NovaSeq----

novaseq_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "Illumina NovaSeq", ]

##### Plot Preprocessing -----

ggplot(novaseq_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_pre.png")

##### Plot Mapping -----

ggplot(novaseq_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_Mapping.png")

##### Plot Assembly -----

ggplot(novaseq_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_Assembly.png")

##### Plot Variant calling -----

ggplot(novaseq_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_vc.png")

##### Plot Consensus -----

ggplot(novaseq_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_Consensus.png")

##### Plot linaje -----

ggplot(novaseq_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_linage.png")


### datos Illumina NovaSeq----

novaseq_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "Illumina NovaSeq", ]

##### Plot Preprocessing -----

ggplot(novaseq_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_pre.png")

##### Plot Mapping -----

ggplot(novaseq_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_Mapping.png")

##### Plot Assembly -----

ggplot(novaseq_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_Assembly.png")

##### Plot Variant calling -----

ggplot(novaseq_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_vc.png")

##### Plot Consensus -----

ggplot(novaseq_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_Consensus.png")

##### Plot linaje -----

ggplot(novaseq_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_novaseq_linage.png")


### datos Ion Torrent----

iont_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "Ion Torrent", ]

##### Plot Preprocessing -----

ggplot(iont_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_it_pre.png")

##### Plot Mapping -----

ggplot(iont_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_it_Mapping.png")

##### Plot Assembly -----

ggplot(iont_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_it_Assembly.png")

##### Plot Variant calling -----

ggplot(iont_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_it_vc.png")

##### Plot Consensus -----

ggplot(iont_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_it_Consensus.png")

##### Plot linaje -----

ggplot(iont_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_it_linage.png")

### datos MinION ----

minion_data<- qc_analysis_bio[qc_analysis_bio$sequencing_platforms_mod == "MinION", ]

##### Plot Preprocessing -----

ggplot(minion_data, aes(x = Preprocessing)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Preprocessing software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_minion_pre.png")

##### Plot Mapping -----

ggplot(minion_data, aes(x = Mapping)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Mapping software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_minion_Mapping.png")

##### Plot Assembly -----

ggplot(minion_data, aes(x = Assembly)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Assembly software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_minion_Assembly.png")

##### Plot Variant calling -----

ggplot(minion_data, aes(x = `Variant Calling`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Variant calling software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_minion_vc.png")

##### Plot Consensus -----

ggplot(minion_data, aes(x = Consensus)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Consensus software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_minion_Consensus.png")

##### Plot linaje -----

ggplot(minion_data, aes(x = `Linage identification`)) +
  geom_bar(width = 0.7) +
  labs(x = "", y = "",  title = "Linage identification software") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Graficos/qc_bioinfo_minion_linage.png")

