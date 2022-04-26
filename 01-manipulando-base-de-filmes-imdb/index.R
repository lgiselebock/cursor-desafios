

# PACOTES NECESSARIOS -----------------------------------------------------

library(tidyverse)


# IMPORTACAO DA BASE ------------------------------------------------------

imdb <- read_rds("01-manipulando-base-de-filmes-imdb/data/imdb.rds")


# MANIPULACAO -------------------------------------------------------------

imdb_longo <- imdb |>
  dplyr::mutate(lucro = orcamento - receita) |>
  tidyr::pivot_longer(
    cols = starts_with("ator_"),
    names_to = "ordem_protagonismo",
    values_to = "ator_atriz"
  )


imdb_atores_parcial <- imdb_longo |>
  tidyr::drop_na(ator_atriz, ano) |>
  dplyr::group_by(ator_atriz) |>
  dplyr::summarise(
    nota_media_imdb = round(mean(nota_imdb, na.rm = TRUE), 1),
    media_lucro = mean(lucro, na.rm = TRUE),
    primeiro_registro = min(ano, na.rm = TRUE),
    ultimo_registro = max(ano, na.rm = TRUE)
  )


filmes <- imdb_longo |>
  dplyr::select(-c(likes_facebook, ordem_protagonismo)) |>
  dplyr::relocate(nota_imdb, .after = classificacao) |>
  dplyr::group_by(ator_atriz) |>
  tidyr::nest() |>
  dplyr::rename(filmes = data)


filmes |>
  dplyr::filter(ator_atriz == "Adam Sandler") |>
  View()


top_generos <- imdb_longo |>
  tidyr::separate_rows(generos, sep = "\\|") |>
  dplyr::group_by(ator_atriz, generos) |>
  dplyr::count() |>
  dplyr::group_by(ator_atriz) |>
  dplyr::arrange(ator_atriz, desc(n)) |>
  dplyr::slice_head(n = 3) |>
  dplyr::mutate(ordem = dplyr::row_number(),
                ordem_nome = stringr::str_glue("top{ordem}_genero")) |>
  tidyr::pivot_wider(
    id_cols = ator_atriz,
    names_from = ordem_nome,
    values_from = generos
  )


imdb_longo_filme_elenco <- imdb_longo |>
  dplyr::select(ator_atriz, titulo)


contracenou <- imdb_longo_filme_elenco |>
  dplyr::inner_join(imdb_longo_filme_elenco, by = "titulo") |>
  dplyr::filter(ator_atriz.x != ator_atriz.y) |>
  dplyr::rename(ator_atriz = ator_atriz.x) |>
  dplyr::group_by(ator_atriz) |>
  dplyr::summarise(contracenou = knitr::combine_words(
    unique(ator_atriz.y),
    and = " e ",
    oxford_comma = FALSE
  ))



# JUNTANDO TODAS AS BASES -------------------------------------------------

imdb_atores <- imdb_atores_parcial |>
  dplyr::left_join(top_generos, by = "ator_atriz") |>
  dplyr::left_join(filmes, by = "ator_atriz") |>
  dplyr::left_join(contracenou, by = "ator_atriz") |>
  dplyr::relocate(dplyr::ends_with("genero"), .after = media_lucro)


