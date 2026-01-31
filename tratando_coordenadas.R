# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

coord <- readxl::read_xlsx("coordenadas_parcelas.xlsx")

## Visualizando ----

coord |> as.data.frame()

coord |> dplyr::glimpse()

# Tratando ----

coord %<>%
  dplyr::mutate(Longitude = Longitude / 1000000,
                Latitude = Latitude / 1000000,
                Parcela = Parcela |> stringr::str_remove("_seca"))

coord

# Exportando ----

coord |> writexl::write_xlsx("coord_trat.xlsx")
