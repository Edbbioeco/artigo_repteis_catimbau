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

df_hill <- comp |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::renyi(scales = 0:1, hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Parcela" = 1,
                "q = 0" = 2,
                "q = 1" = 3) |>
  dplyr::mutate(Season = Parcela |>
                  stringr::str_extract("(?<=_).*"),
                Season = dplyr::case_when(Season == "chuva" ~ "Rainy",
                                          .default = "Dry"))

df_hill

# Modelo linear de riqueza -----

## Criando o modelo ----

modelo_q0 <- glm(`q = 0` ~ Season,
                 data = df_hill,
                 family = poisson(link = "log"))

## Pressupostos do modelo ----

modelo_q0 |> DHARMa::simulateResiduals(plot = TRUE)

## Estatísticas do modelo ----

## Dataframe de estatísticas ----

# Modelo linear de diversidade ----

## Criando o modelo ----

## Pressupostos do modelo ----

## Estatísticas do modelo ----

## Dataframe de estatísticas ----

# Gráfico -----
