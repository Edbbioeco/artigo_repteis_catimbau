# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

# Dados ----

## Importando ----

comp <- readxl::read_xls("composicao.xls")

## Visualizando -----

comp

comp |> dplyr::glimpse()

# Curva de rarefação ----

## Calculando ----

ace_curva <- comp |>
  dplyr::filter(Parcela |> stringr::str_detect("chuva")) |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::estaccumR(permutations = 1000) |>
  summary(display = c("S", "ace"))

ace_curva

## Criando um data frame ----

curva_df <- ace_curva$S |>
  as.data.frame() |>
  dplyr::mutate(tipo = "Observed") |>
  dplyr::rename("Richness" = 2) |>
  dplyr::bind_rows(ace_curva$ace |>
                     as.data.frame() |>
                     dplyr::mutate(tipo = "Estimated") |>
                     dplyr::rename("Richness" = 2))

curva_df

## Gráfico ----

curva_df |>
  ggplot(aes(N, Richness, color = tipo, fill = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 5, shape = 21, color = "black", stroke = 1) +
  scale_y_continuous(breaks = seq(1, 16, 1),
                     limits = c(1, 16)) +
  scale_x_continuous(breaks = seq(1, 6, 1),
                     limits = c(1, 6)) +
  scale_color_manual(values = c("royalblue", 'orange')) +
  scale_fill_manual(values = c("royalblue", 'orange')) +
  labs(fill = NULL,
       color = NULL,
       x = "Number of samples") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom",
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_curva_rarefacao.png",
       height = 10, width = 12)
