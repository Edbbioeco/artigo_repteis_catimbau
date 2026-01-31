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
