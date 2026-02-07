# Pacote ----

library(gert)

# Listando arquivos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Limpando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_mixed()

# Adicionando arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para os comandos de Git")

# Pushando -----

## Privado ----

gert::git_push(remote = "privado", force = TRUE)

## Público ----

gert::git_push(remote = "publico", force = TRUE)

# Pullando ----

## Privado ----

gert::git_pull(remote = "privado")

## Público ----

gert::git_pull(remote = "publico")

# Resetando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_mixed()

# Removendo arquivos do repositório público ----

## Preparando os arquivos ----

gert::git_rm(list.files(pattern = ".xls$|.png$|.xlsx$"))

## Commitando ----

gert::git_commit("Removendo")

## Pushando ----

gert::git_push(remote = "publico", force = TRUE)

## Pullando ----

gert::git_pull(remote = "publico")
