# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(fields)

library(ggtext)

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

dis_bray_seca <- comp |>
  dplyr::filter(Parcela |> stringr::str_detect("seca")) |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::vegdist() |>
  as.numeric()

dis_bray_seca

# Distância geográfica ----

dis_geo <- coord |>
  dplyr::select(dplyr::where(is.numeric)) |>
  as.data.frame() |>
  fields::rdist.earth(miles = FALSE) |>
  as.dist() |>
  as.numeric()

dis_geo

# Teste de Mantel ----

## Chuva ----

### Calculando ----

mantel_chuva <- cor.test(dis_geo, dis_bray_seca)

mantel_chuva

### Estatísticas ----

sts_mantel_chuva <- tibble::tibble(`Distância geográfica` = dis_geo |> median(),
                                   `Diversidade beta` = 0.5,
                                   sts = paste0("t<sub>",
                                                mantel_chuva$parameter,
                                                "</sub> = ",
                                                mantel_chuva$statistic |> round(2),
                                                ", r = ",
                                                mantel_chuva$estimate |> round(2),
                                                ", p ",
                                                dplyr::if_else(mantel_chuva$p.value < 0.01,
                                                               "< 0.01",
                                                               paste0("= ", mantel_chuva$p.value |> round(3)))),
                                   Season = "Rainy")

sts_mantel_chuva

## Seca ----

### Calculando ----

### Estatísticas ----

## Dataframe das estatísticas ----

## Gráfico ----
