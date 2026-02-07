# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(magrittr)

library(ggvegan)

library(ggtext)

library(ggview)

# Dados ----

## Importando ----

comp <- readxl::read_xls("composicao.xls")

## Visualizando -----

comp

comp |> dplyr::glimpse()

# PERMANOVA ----

## Calculando a matriz de disimilaridade ----

dis_bray <- comp |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::vegdist()

dis_bray

## Criando uma variável de sazonaliadde ----

comp %<>%
  dplyr::mutate(Season = dplyr::case_when(Parcela |>
                                            stringr::str_detect("chuva") ~ "Rainy",
                                          .default = "Dry"))

comp

comp |> dplyr::glimpse()

## Calculando PERMANOVA ----

permanova <- vegan::adonis2(dis_bray ~ Season,
                            data = comp,
                            permutations = 1000)

permanova

# NMDS ----

## Calculando NMDS ----

nmds <- dis_bray |> vegan::metaMDS(distance = "bray")

nmds

## Gráfico ----

### Scores ----

scores_nmds <- nmds |>
  vegan::scores() |>
  as.data.frame() |>
  dplyr::mutate(Season = comp$Season)

scores_nmds

### Estatísticas da PERMONOVA ----

sts_nmds <- tibble::tibble(NMDS1 = scores_nmds$NMDS1 |> median(),
                           NMDS2 = 0.8,
                           sts = paste0("pseudo-F<sub>",
                                        permanova$Df[1],
                                        ", ",
                                        permanova$Df[2],
                                        "</sub> = ",
                                        permanova$F[1] |> round(2),
                                        ", p ",
                                        dplyr::if_else(permanova$`Pr(>F)`[1] < 0.01,
                                                       "< 0.01",
                                                       paste0("= ",
                                                              permanova$`Pr(>F)`[1])),
                                        ", R² = ",
                                        permanova$R2[1] |> round(2)),
                           Season = NA)

sts_nmds

### Gráfico ----

scores_nmds |>
  ggplot(aes(NMDS1, NMDS2, fill = Season)) +
  geom_point(shape = 21, size = 7.5, color = "black", stroke = 1) +
  scale_fill_manual(values = c("orange", "royalblue"),
                    guide = guide_legend(title.position = "top",
                                         title.hjust = 0.5)) +
  ggtext::geom_richtext(data = sts_nmds,
                        aes(NMDS1, NMDS2, label = sts),
                        size = 10,
                        label.colour = NA,
                        fill = NA) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom",
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_nmds.png",
       height = 10, width = 12)
