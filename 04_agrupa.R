#!/usr/bin/env Rscript

library("magrittr")
library("ggplot2")
library("dplyr")
library("ggridges")
library("ggplot2")
library("purrr")
library("kableExtra")
library("readr")

load("./99_output/estudo_B3.RData")

formato <- "svg"
resolucao <- "retina"

print("Carteiras amostradas")
print(portfolios)

# Constrói os gráficos de ridgeline e de linhas para o grupo de interesse
graficos <- function(grupo_alvo, nome_arquivo, d = formato, r = resolucao) {
    betas_ridgeline <- list()
    betas_acf <- list()
    betas_pacf <- list()
    for (i in 1:length(grupo_alvo)) {
        betas_ridgeline[[i]] <- cross_join(grupo_alvo[[i]]$argumentos, grupo_alvo[[i]]$betas)
        betas_acf[[i]] <- cross_join(grupo_alvo[[i]]$argumentos, grupo_alvo[[i]]$acf)
        betas_pacf[[i]] <- cross_join(grupo_alvo[[i]]$argumentos, grupo_alvo[[i]]$pacf)
    }
    
    betas_ridgeline <- bind_rows(betas_ridgeline)
    betas_acf <- bind_rows(betas_acf)
    betas_pacf <- bind_rows(betas_pacf)
    
    caption <- paste(
        "Parâmetros:",
        "frequência =", betas_ridgeline %>% pull(.interval) %>% unique,
        "| mercado =", betas_ridgeline %>% pull(.market_ticker) %>% unique,
        "| carteira =", betas_ridgeline %>% pull(id_carteira) %>% unique
        )
    
    ggplot(betas_ridgeline, aes(x = beta, y = as.factor(.observations), fill = .observations, group = as.factor(.observations))) +
        geom_density_ridges() +
        theme_ridges() + 
        labs(y = "Tamanho de Janela", x = "Beta Estimado", title = "Densidade de Beta", caption = caption) +
        geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.2) +
        theme(legend.position = "none")
    ggsave(filename = paste("./99_output/ridgeline_", nome_arquivo, ".", d, sep = ""), device = d, bg = "white", dpi = r)
    
    ggplot(betas_ridgeline, aes(x = date, y = beta, color = as.factor(.observations))) +
        geom_line(linewidth = 0.5) +
        geom_point(size = 1.5) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "orange", linewidth = 0.5) +
        labs(title = "Série Temporal de Betas Estimados", x = "Data", y = "Beta Estimado", caption = caption) +
        scale_color_discrete("Tamanho de Janela") +
        theme(legend.position = "bottom")
    ggsave(filename = paste("./99_output/ts_", nome_arquivo, ".", d, sep = ""), device = d, bg = "white", dpi = r)
    
    ggplot(betas_acf, aes(x = lag, y = acf, fill = as.factor(.observations), group = as.factor(.observations))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "orange") +
        labs(
            title = "Função de Autocorrelação",
            x = "Lag",
            y = "Autocorrelação",
            caption = caption
        ) +
        scale_color_discrete("Tamanho de Janela") +
        theme(legend.position = "bottom")
    ggsave(filename = paste("./99_output/acf_", nome_arquivo, ".", d, sep = ""), device = d, bg = "white", dpi = r)
    
    ggplot(betas_pacf, aes(x = lag, y = pacf, fill = as.factor(.observations), group = as.factor(.observations))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "orange") +
        labs(
            title = "Função de Autocorrelação Parcial",
            x = "Lag",
            y = "Autocorrelação",
            caption = caption
        ) +
        scale_color_discrete("Tamanho de Janela") +
        theme(legend.position = "bottom")
    ggsave(filename = paste("./99_output/pacf_", nome_arquivo, ".", d, sep = ""), device = d, bg = "white", dpi = r)
}

iter <- tibble(expand.grid(
    id_carteira = 10:20,
    .interval = c("week", "month"),
    .market_ticker = c("^BVSP", "^GSPC")
))

for (i in 1:nrow(iter)) {
    row <- slice(iter, i)
    
    g <- purrr::keep(
        betas_iterados,
        ~ .$argumentos$id_carteira == row$id_carteira
        & .$argumentos$.interval == row$.interval
        & .$argumentos$.market_ticker == row$.market_ticker
    )
    
    suffix <- paste(row$id_carteira, row$.interval, row$.market_ticker, sep = "_")
    graficos(g, suffix)
}

# Monta tabela com resultados dos testes de hipóteses
hipoteses <- list()
for (i in 1:length(betas_iterados)) {
    p <- tibble(
        shapiro_w = betas_iterados[[i]]$shapiro$statistic,
        shapiro_p = betas_iterados[[i]]$shapiro$p.value,
        adf_stat = betas_iterados[[i]]$adf$statistic,
        adf_lag_order = betas_iterados[[i]]$adf$parameter,
        adf_p = betas_iterados[[i]]$adf$p.value
    )
    
    hipoteses[[i]] <- cross_join(betas_iterados[[i]]$argumentos, p)
}
hipoteses <- bind_rows(hipoteses) %>%
    arrange(id_carteira, .market_ticker, .interval, .observations)

hipoteses %>%
    select(id_carteira, .market_ticker, .interval, .observations, shapiro_p, adf_p) %>%
    kable(format = "latex", booktabs = TRUE) %>%
    writeLines("./99_output/hipoteses.tex")

write_csv(hipoteses, "./99_output/hipoteses.csv")

hipoteses %>%
    select(id_carteira, .market_ticker, .interval, .observations, shapiro_p, adf_p) %>%
    mutate(shapiro_p = round(shapiro_p, 3), adf_p = round(adf_p, 3)) %>%
    print(n = 400)