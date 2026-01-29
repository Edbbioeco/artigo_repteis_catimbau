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

usethis::use_git_remote(name = "artigo_lagartos",
                        url = "https://github.com/Edbbioeco/artigo_repteis_catimbau.git")

# listando os remotes ----

usethis::git_remotes()

# Adicionado o branch main ----

usethis::git_default_branch_configure()
