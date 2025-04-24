#!/usr/bin/env Rscript

library("magrittr")

load("./99_output/estudo_B3.RData")

set.seed(4)

tamanho_portfolios <- c(
    1,
    5,
    10,
    20
)
qtd_portfolios <- 5
n <- rep.int(tamanho_portfolios, qtd_portfolios) %>% sort

portfolios <- sapply(n, sample, x = tickers_yf)

save.image("./99_output/estudo_B3.RData")
