#!/usr/bin/env Rscript

library("furao")
library("ggridges")
library("magrittr")
library("tibble")
library("dplyr")
library("purrr")

load("./99_output/estudo_B3.RData")

# constrói conjuntos de parâmetros, para as combinações possíveis
frequencias <- tibble::tibble(
    .interval = c(rep("week", 4), rep("month", 4)),
    .observations = c(13, 26, 52, 104, 3, 6, 12, 24)
)
carteiras <- tibble::tibble(
    .portfolio_tickers = portfolios,
    id_carteira = 1:length(portfolios)
)
mercados <- tibble::tibble(.market_ticker = c(nm_indice_ibov, nm_indice_ext))

params <- frequencias %>%
    dplyr::cross_join(carteiras) %>%
    dplyr::cross_join(mercados) %>%
    dplyr::arrange(id_carteira, .interval, .observations, .market_ticker)

dim(params)

# cálculo em paralelo (pode ser demorado - aproveite para se levantar e tomar um café)
betas_iterados <- purrr::pmap(
    params %>% select(- id_carteira),
    purrr::partial(get_rolling_betas, data = retornos_yf)
)
length(betas_iterados)

for (i in 1:length(betas_iterados)) {
    betas_iterados[[i]]$argumentos <- params %>% slice(i)
}

save.image("./99_output/estudo_B3.RData")