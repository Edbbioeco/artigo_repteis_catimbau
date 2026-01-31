# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

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

## Criando uma variável de sazonaliadde ----

## Calculando PERMANOVA ----

## Estatísticas da PERMONOVA ----

# NMDS ----

## Calculando NMDS ----

## Gráfico ----
