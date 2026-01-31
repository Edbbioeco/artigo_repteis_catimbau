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
