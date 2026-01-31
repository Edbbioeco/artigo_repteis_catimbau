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

mantel_chuva <- cor.test(dis_geo, dis_bray_chuva)

mantel_chuva

### Estatísticas ----

sts_mantel_chuva <- tibble::tibble(`Geographic distance (Km)` = dis_geo |> median(),
                                   `Beta diversity` = 0.475,
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

mantel_seca <- cor.test(dis_geo, dis_bray_seca)

mantel_seca

### Estatísticas ----

sts_mantel_seca <- tibble::tibble(`Geographic distance (Km)` = dis_geo |> median(),
                                  `Beta diversity` = 0.725,
                                  sts = paste0("t<sub>",
                                               mantel_seca$parameter,
                                               "</sub> = ",
                                               mantel_seca$statistic |> round(2),
                                               ", r = ",
                                               mantel_seca$estimate |> round(2),
                                               ", p ",
                                               dplyr::if_else(mantel_seca$p.value < 0.01,
                                                              "< 0.01",
                                                              paste0("= ",
                                                                     mantel_chuva$p.value |> round(3)))),
                                 Season = "Dry")

sts_mantel_seca

## Dataframe das estatísticas ----

df_sts <- dplyr::bind_rows(sts_mantel_chuva,
                           sts_mantel_seca)

df_sts

## Gráfico ----

tibble::tibble(`Geographic distance (Km)` = dis_geo,
               Rainy = dis_bray_chuva,
               Dry = dis_bray_seca) |>
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "Season",
                      values_to = "Beta diversity") |>
  ggplot(aes(`Geographic distance (Km)`, `Beta diversity`,
             fill = Season, color = Season)) +
  geom_point(shape = 21, size = 7.5, color = "black", stroke = 1) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  scale_color_manual(values = c("orange4", "royalblue4")) +
  ggtext::geom_richtext(data = df_sts,
                        aes(`Geographic distance (Km)`, `Beta diversity`,
                            label = sts),
                        color = "black",
                        size = 10,
                        label.colour = NA,
                        fill = NA) +
  geom_smooth(data = . %>%
                dplyr::filter(Season == "Dry"),
              method = "lm", se = FALSE) +
  facet_wrap(~Season, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2.4, 12.4, 2)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_mantel_sazonalidade.png",
       height = 10, width = 12)

