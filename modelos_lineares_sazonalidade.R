# Pacotes -----

library(readxl)

library(tidyverse)

library(vegan)

library(DHARMa)

library(ggtext)

library(performance)

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

# Riqueza e diversidade ----

## Dataframe único das variáveis ambientais ----

var_df <- var_macro |>
  dplyr::left_join(var_micro |>
                     dplyr::select(1:3),
                   by = "Parcela")

var_df

## Riqueza ----

riqueza

## Diversidade ----

## Unindo os dados ----

# Modelo linear de riqueza -----

## Criando o modelo ----

## Pressupostos do modelo ----

## Estatísticas do modelo ----

## Dataframe de estatísticas ----

## Gráfico -----

# Modelo linear de diversidade ----

## Criando o modelo ----

## Pressupostos do modelo ----

## Estatísticas do modelo ----

## Dataframe de estatísticas ----

## Gráfico -----
