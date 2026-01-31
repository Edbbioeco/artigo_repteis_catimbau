# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(sf)

library(ggview)

# Dados ----

## Composição de espécies ----

### Importando ----

comp <- readxl::read_xls("composicao.xls")

### Visualizando -----

comp

comp |> dplyr::glimpse()

## Coordenada das parcelas ----

### Importando ----

coord <- readxl::read_xlsx("coord_trat.xlsx")

### Visualizando ----

coord |> as.data.frame()

coord |> dplyr::glimpse()

# Dissimilaridade da composição ----

## Época chuvosa ----

dis_bray_chuva <- comp |>
  dplyr::filter(Parcela |> stringr::str_detect("chuva")) |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::vegdist() |>
  as.numeric()

dis_bray_chuva

## Época seca ----

# Distância geográfica ----

## Criando um shapefile das coordenadas ----

coord_sf <- coord |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 32725)

coord_sf
