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

pseudor2_q0

## Dataframe de estatísticas ----

sts_q0 <- tibble::tibble(Diversity = 10,
                         tipo = "q = 0",
                         Season = 0.5,
                         sts = paste0("β1 ± EP = ",
                                      summary_q0$coefficients[2, 1] |> round(3),
                                      " ± ",
                                      summary_q0$coefficients[2, 2] |> round(4),
                                      "<br>z = ",
                                      summary_q0$coefficients[2, 3] |> round(2),
                                      ", p = ",
                                      summary_q0$coefficients[2, 4] |> round(3),
                                      ", pseudo-R² = ",
                                      pseudor2_q0[[2]] |> round(2)))

sts_q0

# Modelo linear de diversidade ----

## Criando o modelo ----

modelo_q1 <- lm(q_1 ~ season,
                data = df_hill |> janitor::clean_names())

## Pressupostos do modelo ----

modelo_q1 |> performance::check_model(check = c("homogeneity",
                                                "qq",
                                                "normality"))

modelo_q1 |> performance::check_heteroscedasticity()

modelo_q1 |> performance::check_normality()

## Estatísticas do modelo ----

summary_q1 <- modelo_q1 |> summary()

summary_q1

## Dataframe de estatísticas ----

sts_q1 <- tibble::tibble(Diversity = 5,
                         tipo = "q = 1",
                         Season = 0.5,
                         sts =paste0("β1 ± EP = ",
                                     summary_q1$coefficients[2, 1] |> round(3),
                                     " ± ",
                                     summary_q1$coefficients[2, 2] |> round(4),
                                     "<br>t = ",
                                     summary_q1$coefficients[2, 3] |> round(2),
                                     ", p = ",
                                     summary_q1$coefficients[2, 4] |> round(3),
                                     "<br> F<sub>",
                                     summary_q1$fstatistic[2] |>
                                       round(1),
                                     ", ",
                                     summary_q1$fstatistic[3] |>
                                       round(1),
                                     "</sub> = ",
                                     summary_q1$fstatistic[1] |>
                                       round(2),
                                     ", p = ",
                                     pf(summary_q1$fstatistic[1],
                                        summary_q1$fstatistic[2],
                                        summary_q1$fstatistic[3],
                                        lower.tail = FALSE) |>
                                       round(2),
                                     ", R² = ",
                                     summary_q1$adj.r.squared |>
                                       round(2)))

sts_q1

# Gráfico -----

## Unindo os dataframes de estatística ----

## Gráfico ----
