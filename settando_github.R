# Pacotes ----

library(usethis)

# iniciando ----

usethis::use_git()

# configurando usuário ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# informando o projeto ----

usethis::proj_get()

# Informando o repositório ----

## Público ----

usethis::use_git_remote(name = "publico",
                        url = "https://github.com/Edbbioeco/artigo_repteis_catimbau.git",
                        overwrite = TRUE)

## Privado ----

usethis::use_git_remote(name = "privado",
                        url = "https://github.com/Edbbioeco/artigo_lgartos_catimbau_privado.git",
                        overwrite = TRUE)

# listando os remotes ----

usethis::git_remotes()

# Adicionado o branch main ----

usethis::git_default_branch_configure()
