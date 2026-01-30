# Pacotes -----

library(readxl)

library(tidyverse)

library(vegan)

library(reshape2)

library(glmmTMB)

library(janitor)

library(DHARMa)

library(ggtext)

library(ggview)

# Dados ----

## Composição de espécies ----

### Importando ----

comp <- readxl::read_xls("composicao.xls")

### Visualizando -----

comp

comp |> dplyr::glimpse()

## Variáveis macroclimáticas ----

### Importando ----

var_micro <- readxl::read_xls("var_micro.xls")

### Visualizando -----

var_micro

var_micro |> dplyr::glimpse()

## Variáveis microclimáticas ----

### Importando ----

var_macro <- readxl::read_xls("var_macro.xls")

### Visualizando -----

var_macro

var_macro |> dplyr::glimpse()

# Dissimilaridades ----

## Dissimilaridade de composição ----

comp |>
  tibble::column_to_rownames(var = "Parcelas") |>
  vegan::vegdist()
