# Pacotes -----

library(readxl)

library(tidyverse)

library(vegan)

library(janitor)

library(DHARMa)

library(performance)

library(ggtext)

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

# Riqueza e diversidade ----

df_hill <- comp |>
  tibble::column_to_rownames(var = "Parcela") |>
  vegan::renyi(scales = 0:1, hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Parcela" = 1,
                "q = 0" = 2,
                "q = 1" = 3) |>
  dplyr::mutate(Season = Parcela |>
                  stringr::str_extract("(?<=_).*"),
                Season = dplyr::case_when(Season == "chuva" ~ "Rainy",
                                          .default = "Dry"))

df_hill

# Modelo linear de riqueza -----

## Criando o modelo ----

modelo_q0 <- glm(q_0 ~ season,
                 data = df_hill |> janitor::clean_names(),
                 family = poisson(link = "log"))

## Pressupostos do modelo ----

modelo_q0 |> DHARMa::simulateResiduals(plot = TRUE)

## Estatísticas do modelo ----

summary_q0 <- modelo_q0 |> summary()

summary_q0

## Pseudo-R² ----

pseudor2_q0 <- modelo_q0 |> performance::r2_mcfadden()

## Dataframe de estatísticas ----

sts_q0 <- tibble::tibble(Diversity = 10,
                         Season = 0.5,
                         sts = paste0("β1 ± EP = ",
                                      summary_q0$coefficients[2, 1] |> round(3),
                                      " ± ",
                                      summary_q0$coefficients[2, 2] |> round(4),
                                      "<br>t = ",
                                      summary_q0$coefficients[2, 3] |> round(2),
                                      ", p = ",
                                      summary_q0$coefficients[2, 4] |> round(3),
                                      ", pseudo-R² = ",
                                      pseudor2_q0 |> round(2)))

# Modelo linear de diversidade ----

## Criando o modelo ----

## Pressupostos do modelo ----

## Estatísticas do modelo ----

## Dataframe de estatísticas ----

# Gráfico -----

## Unindo os dataframes de estatística ----

## Gráfico ----
