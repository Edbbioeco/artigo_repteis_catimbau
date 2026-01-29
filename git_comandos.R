# Pacote ----

library(gert)

# Listando arquivos ----

gert::git_status() |>
  as.data.frame()

# Adicionando arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para os comandops de Git")

# Pushando -----

gert::git_push(remote = "artigo_lagartos", force = TRUE)

# Pullando ----

gert::git_pull(remote = "artigo_lagartos")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft("HEAD^")
