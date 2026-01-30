# Pacotes -----

library(readxl)

library(tidyverse)

library(vegan)

library(reshape2)

library(viridis)

library(glmmTMB)

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

  assign(paste0("dis_", nome_var),
         dissim_amb,
         envir = globalenv())

}

nome_var <- df_var |>
  dplyr::select(dplyr::where(is.numeric)) |>
  names()

nome_var

purrr::map(nome_var, amb_dis)

## Unindo os dados ----

df_dis <- ls(pattern = "dis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_cols()

df_dis

df_dis |> dplyr::glimpse()

# Multicolinearidade ----

## Calculando a correlação múltipla ----

cor_multipla <- df_dis |>
  dplyr::select(!dplyr::contains("comp")) |>
  cor(method = "spearman") |>
  as.matrix()

cor_multipla

## Dataframe dos valores de correlação ----

cor_multipla[upper.tri(cor_multipla)] <- NA

cor_multipla

cor_df <- cor_multipla |>
  reshape2::melt() |>
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                         .default = "não"),
                value = value |> round(2),
                Var1 = Var1 |>
                  stringr::str_replace_all("_", " ") |>
                  stringr::word(2),
                Var2 = Var2 |>
                  stringr::str_replace_all("_", " ") |>
                  stringr::word(2)) |>
  dplyr::filter(!value |> is.na() & igual == "não") |>
  dplyr::select(-igual) |>
  dplyr::rename("Spearman Correlation Index" = value)

cor_df

## Gráfico ----

cor_df |>
  ggplot(aes(Var1, Var2,
             fill = `Spearman Correlation Index`,
             label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(color = "black", size = 7.5, fontface = "bold") +
  labs(x = NULL,
       y = NULL) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       guide = guide_colorbar(title.position = "top",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 1),
                       limits = c(-1, 1)) +
  coord_equal() +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 20),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "multicolinearidade.png",
       height = 10, width = 12)

# Modelos lineares ----

## Nome das variáveis ----

nomes_var <- df_dis |>
  names() |>
  stringr::str_replace_all("_", " ") |>
  stringr::word(2)

nomes_var

names(df_dis) <- nomes_var

var <- df_dis |>
  dplyr::select(-comp) |>
  names()

var

## Criando os modelos ----

criar_modelos <- function(var){

  modelo <- glmmTMB::glmmTMB(comp ~ df_dis[[var]],
                             data = df_dis,
                             family = glmmTMB::beta_family())

  assign(paste0("modelo_", var),
         modelo,
         envir = globalenv())

}

purrr::map(var, criar_modelos)

ls(pattern = "modelo_") |>
  mget(envir = globalenv())

## Pressupostos dos modelos ----

## Estatísticas dos modelos ----
