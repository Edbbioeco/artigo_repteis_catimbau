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

dis_comp <- comp |>
  dplyr::mutate(Parcela = Parcela |> stringr::str_remove_all("_chuva|_seca")) |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~max(.)),
                   .by = Parcela) |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::vegdist() |>
  as.numeric()

dis_comp

## Dissimilaridade ambiental ----

df_var <- var_micro |>
  dplyr::select(1:3) |>
  dplyr::left_join(var_macro,
                   by = "Parcela")

df_var

amb_dis <- function(nome_var){

  dissim_amb <- df_var |>
    dplyr::mutate(Parcela = Parcela |> stringr::str_remove_all("_Chuvosa|_Seca")) |>
    dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                   .fns = ~max(.)),
                     .by = Parcela) |>
    dplyr::select(nome_var) |>
    vegan::vegdist(method = "euclidean") |>
    as.numeric()

  assign(paste0("dis_amb_", nome_var),
         dissim_amb,
         envir = globalenv())

}

nome_var <- df_var |>
  dplyr::select(dplyr::where(is.numeric)) |>
  names()

nome_var
