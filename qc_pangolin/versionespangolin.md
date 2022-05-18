## Versiones de Pangolin

Hemos usado las siguientes versiones de Pangolin para discernir si existen diferencias a nivel de versión, análisis bioinformático y secuenciación:

- v3.1.16
- v3.1.17
- v3.1.18
- v3.1.19
- v3.1.20

Dividiremos el análisis por muestras para 27 laboratorios (todos Illumina) analizados con las versiones anteriores de Pangolin con sus análisis y el análisis propio de la Unidad de Bioinformática (Viralrecon).

### Muestra 1

Los resultados son homogeneos para todos los casos, con el linaje B.1.1.7 menos para los siguientes casos:

- **COD_2122, v3.1.17**: Q.4
Segun [COGUK](https://cov-lineages.org/lineage_list.html) es un sublinaje de la B.1.1.7 con una mutación en la spike (spike:H681R). El issue relacionado [Pango_designation: 176](https://github.com/cov-lineages/pango-designation/issues/176) y añadido a la versión v1.2.57 de pango-designation [Pango_designation v1.2.57](https://github.com/cov-lineages/pango-designation/releases/tag/v1.2.57).

Parece que es un problema del PANGOLearn (Tabla X).

| taxon    | sample | class        | pangolin_version | lineage | conflict | ambiguity_score | scorpio_call         | scorpio_support | scorpio_conflict | version         | pangoLEARN_version | pango_version | status    | note                                                                      |
|----------|--------|--------------|------------------|---------|----------|-----------------|----------------------|-----------------|------------------|-----------------|--------------------|---------------|-----------|---------------------------------------------------------------------------|
| COD_2122 |      1 | laboratories | 3.1.16           | B.1.1.7 | 0        | 0.8388268881    | Alpha (B.1.1.7-like) | 0.8261          | 0.0435           | PLEARN-v1.2.97  | 2021-11-18         | v1.2.97       | passed_qc | scorpio call: Alt alleles 19; Ref alleles 1; Amb alleles 3; Oth alleles 0 |
| COD_2122 |      1 | viralrecon   | 3.1.16           | B.1.1.7 | 0        | 0.9135502671    | Alpha (B.1.1.7-like) | 0.8261          | 0.1304           | PLEARN-v1.2.97  | 2021-11-18         | v1.2.97       | passed_qc | scorpio call: Alt alleles 19; Ref alleles 3; Amb alleles 1; Oth alleles 0 |
| COD_2122 |      1 | laboratories | 3.1.17           | Q.4     |        0 |    0.8523217457 | Alpha (B.1.1.7-like) |           0.913 |           0.0435 | PLEARN-v1.2.101 |         2021-11-25 | v1.2.101      | passed_qc | scorpio call: Alt alleles 21; Ref alleles 1; Amb alleles 1; Oth alleles 0 |
| COD_2122 |      1 | viralrecon   | 3.1.17           | Q.4     |        0 |    0.9174246051 | Alpha (B.1.1.7-like) |          0.8696 |           0.1304 | PLEARN-v1.2.101 |         2021-11-25 | v1.2.101      | passed_qc | scorpio call: Alt alleles 20; Ref alleles 3; Amb alleles 0; Oth alleles 0 |

Las mutaciones observadas para el sublinaje según el issue son las siguientes:

```
- ORF1a:A2320V (nsp3:A1502V, nucleotide C7224T)
- S:H681R (A23604G)
```

He buscado estas dos mutaciones en la longtable de variantes de la muestra 1 de COD_2122 y la he comparado con los resultados de la muestra 1 de COD_2107 (un laboratorio que lo ha hecho todo muy bien). Aparece la mutacion a nivel de nucleótido C23604T en el gen S pero no la mutacion C7224T.

[longtable_variantes_muestra_1](https://docs.google.com/spreadsheets/d/1eq7tpPi9YkRsEP3AQXTR_suEM46nU4i9sURXi6IpCFk/edit#gid=2059206970)
[longtable_outputpangolin_muestra_1](https://docs.google.com/spreadsheets/d/1eq7tpPi9YkRsEP3AQXTR_suEM46nU4i9sURXi6IpCFk/edit#gid=39736565)

- **COD_2137, todas las versiones en Viralrecon**
El output de Pangolin para esas muestras nos dice que aunque pangoLEARN si asigna correctamente el linaje, este no es soportado por scorpio. 

```
Resultado de Pangolin

COD_2137_1_NC_045512.2,None,,,,,,PLEARN-v1.2.123,3.1.20,2022-02-28,v1.2.123,passed_qc,pangoLEARN lineage assignment B.1.1.7 was not supported by scorpio
```
Analizamos los vcf en busca de la posible diferencia entre los fastas procedentes de los laboratorios y los procedentes de Viralrecon.

```
Resultado de una delección compartida entre ambos vcf:

- COD_2137_1: 153 mutaciones

```
MN908947.3      11287   .       GNNNNNNNNN      G       .       PASS    DP=2278 GT:REF_DP:REF_RV:REF_QUAL:ALT_DP:ALT_RV:ALT_QUAL:ALT_FREQ       1:1548:1173:56:1842:0:20:0.808604
MN908947.3      21764   .       ANNNNNN A       .       PASS    DP=729  GT:REF_DP:REF_RV:REF_QUAL:ALT_DP:ALT_RV:ALT_QUAL:ALT_FREQ       1:539:177:52:682:0:20:0.935528
```
- Viralrecon_2137_1: 27 mutaciones

```
NC_045512.2	11287	.	GTCTGGTTTT	G	.	PASS	DP=4822	GT:REF_DP:REF_RV:REF_QUAL:ALT_DP:ALT_RV:ALT_QUAL:ALT_FREQ	1:4803:3113:38:4082:0:20:0.846537
NC_045512.2     21764   .       ATACATG A       .       PASS    DP=1129 GT:REF_DP:REF_RV:REF_QUAL:ALT_DP:ALT_RV:ALT_QUAL:ALT_FREQ       1:1128:442:38:972:0:20:0.860939
```




