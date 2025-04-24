#!/usr/bin/env Rscript

library("readr")
library("dplyr")
library("stringr")
library("yfR")
library("magrittr")

nm_indice_ibov <- "^BVSP"
nm_indice_ext <- "^GSPC"
dt_inicio <- as.Date("2020-01-01")
dt_fim <- as.Date("2024-12-31")

extrai_tickers <- function() {
    fname <- "./00_input/IBOVQuad_1-2025.csv"
    columns <- c(
        "ticker",
        "nm_empresa",
        "tp_acao",
        "qtd_teorica",
        "perc_participacao"
    )
    
    # removendo as 2 ultimas linhas, que não fazem parte da listagem
    tmp_f <- tempfile()
    read_lines(fname) %>%
        head(-2) %>%
        write_lines(tmp_f)
    
    tickers_ibov <- readr::read_delim(tmp_f, delim = ";", skip = 2, col_names = columns) %>%
        dplyr::select(dplyr::all_of(columns)) %>%
        dplyr::mutate(
            yf_ticker = stringr::str_c(ticker, ".SA"),
            yf_pref_ticker = stringr::str_c(ticker, "F.SA")
        )
    
    unlink(tmp_f)
    
    # argumentos p/ o yf_get
    tickers <- tickers_ibov %>%
        dplyr::pull("yf_ticker") %>%
        append(nm_indice_ibov) %>%
        append(nm_indice_ext)
    
    dataset <- yfR::yf_get(
        tickers,
        dt_inicio,
        dt_fim,
        bench_ticker = nm_indice_ibov,
        freq_data = "daily",
        thresh_bad_data = 0.01,
        be_quiet = TRUE
    )
}

if (sys.nframe() == 0) {
    retornos_yf <- extrai_tickers()
    limites <- retornos_yf %>%
        dplyr::select("ref_date") %>%
        dplyr::summarise(inicio = min(ref_date), fim = max(ref_date))
    
    # tickers negociados durante todo o período 
    tickers_yf <- retornos_yf %>%
        dplyr::select("ticker", "ref_date") %>%
        dplyr::summarise(inicio = min(ref_date), fim = max(ref_date), .by = "ticker") %>%
        dplyr::filter(inicio == limites["inicio"] & fim == limites["fim"]) %>%
        dplyr::pull("ticker") %>%
        unique %>%
        subset(!. %in% c(nm_indice_ibov, nm_indice_ext))
    save.image("./99_output/estudo_B3.RData")
}
