# Pacote ----

library(gert)

# Arquivos Públicos ----

## Setando o branche main ----

gert::git_branch_checkout("main")

## Listando arquivos ----

gert::git_status() |>
  as.data.frame()

## Adicionando arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R")) |>
  as.data.frame()

## Commitando ----

gert::git_commit("Script para comandos de Git")

## Pushando -----

### Privado ----

gert::git_push(remote = "privado", force = TRUE)

### Público ----

gert::git_push(remote = "publico", force = TRUE)

## Pullando ----

### Privado ----

gert::git_pull(remote = "privado")

### Público ----

gert::git_pull(remote = "publico")

# Arquivos privados ----

## Branche nova -----

gert::git_branch_create("priv")

gert::git_branch_checkout("priv")

## Listando arquivos ----

gert::git_status() |>
  as.data.frame()

## Adicionando arquivos ----

gert::git_add(list.files(pattern = ".xlsx$")) |>
  as.data.frame()

## Commitando ----

gert::git_commit("Base de dados")

## Pushando -----

gert::git_push(remote = "privado", force = TRUE)

## Pullando ----

gert::git_pull(remote = "privado")

## Merging ----

gert::git_branch_checkout("main")

gert::git_merge("priv")

gert::git_push(remote = "privado", force = TRUE)

gert::git_pull(remote = "privado")

# Resetando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_mixed()

# Removendo arquivos ----

## Preparando os arquivos ----

gert::git_rm(list.files(pattern = ".xls$.|.png$|.xlsx$"))

## Commitando ----

gert::git_commit("Removendo")

## Pushando ----

gert::git_push(remote = "publico", force = TRUE)

## Pullando ----

gert::git_pull(remote = "publico")
