# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(magrittr)

library(ggvegan)

library(ggtext)

library(ggview)

# Dados ----

## Importando ----

comp <- readxl::read_xls("composicao.xls")

## Visualizando -----

comp

comp |> dplyr::glimpse()

# PERMANOVA ----

## Calculando a matriz de disimilaridade ----

dis_bray <- comp |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::vegdist()

dis_bray

## Criando uma variável de sazonaliadde ----

comp %<>%
  dplyr::mutate(Season = dplyr::case_when(Parcela |>
                                            stringr::str_detect("chuva") ~ "Rainy",
                                          .default = "Dry"))

comp

comp |> dplyr::glimpse()

## Calculando PERMANOVA ----

permanova <- vegan::adonis2(dis_bray ~ Season,
                            data = comp,
                            permutations = 1000)

permanova

# NMDS ----

## Calculando NMDS ----

nmds <- dis_bray |> vegan::metaMDS(distance = "bray")

nmds

## Gráfico ----

### Scores ----

### Estatísticas da PERMONOVA ----
