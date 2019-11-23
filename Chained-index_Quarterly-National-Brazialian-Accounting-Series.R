rm(list = ls())
# Libraries ---------------------------------------------------------------
library(openssl)
library(httr)
library(rvest)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(sidrar)
library(dplyr)
library(zoo)
# dados -------------------------------------------------------------------
dados_sidra <- get_sidra(
  api = "/t/1846/n1/all/v/all/p/all/c11255/90707,93404,93405,93406,102880/d/v585%200")
# ETL ---------------------------------------------------------------------
CNT <- dados_sidra %>%
  select(`Setores e subsetores`, Trimestre, Valor) %>%
  mutate(Var = str_replace_all(
    `Setores e subsetores`, c("PIB a preços de mercado" = "PIB",
                              "Despesa de consumo das famílias" = "C",
                              "Despesa de consumo da administração pública" = "G",
                              "Formação bruta de capital fixo" = "FBKF",
                              "Variação de estoque" = "var.E"))) %>%
  mutate(Ano = as.integer(str_sub(Trimestre, start = -4))) %>%
  mutate(Tri = as.integer(str_sub(Trimestre, end = 1))) %>%
  filter(Ano >= 2000) %>%
  select(Var, Ano, Tri, Valor)
l_CNT <- split(CNT, CNT$Var)
I_macro <- cbind(
  l_CNT$PIB["Ano"],
  l_CNT$PIB["Tri"],
  l_CNT$PIB["Valor"],
  l_CNT$C["Valor"],
  c(l_CNT$FBKF["Valor"] + l_CNT$var.E["Valor"]),
  l_CNT$G["Valor"]
)
names(I_macro) <- c("ano", "tri", "PIB", "C", "I", "G")
chained <- data.frame(PIB = rep(0, 78), C = rep(0, 78), I = rep(0, 78), G = rep(0, 78))
for (i in 1:(nrow(I_macro)-4)) {
  chained$PIB[i+4] <- ((I_macro[i+4, "PIB"]-I_macro[i, "PIB"])/I_macro[i+4, "PIB"])*100
  chained$C[i+4] <- ((I_macro[i+4, "C"]-I_macro[i, "C"])/I_macro[i+4, "C"])*100
  2
  chained$I[i+4] <- ((I_macro[i+4, "I"]-I_macro[i, "I"])/I_macro[i+4, "I"])*100
  chained$G[i+4] <- ((I_macro[i+4, "G"]-I_macro[i, "G"])/I_macro[i+4, "G"])*100
}
dados_ts <- ts(chained, start = c(I_macro$ano[1], I_macro$tri[1]), freq = 4)
# Visualization -----------------------------------------------------------
ggplot(dados_ts, aes(seq(as.Date("2000/1/1"), by = "quarter", length.out = 78)))+
  geom_line(aes(y = dados_ts[, "PIB"]), size = 1.1, color = "darkblue")+
  geom_line(aes(y = dados_ts[, "C"]), size = 1.1, color = "darkgreen")+
  geom_line(aes(y = dados_ts[, "I"]), size = 1.1, color = "cadetblue3")+
  geom_line(aes(y = dados_ts[, "G"]), size = 1.1, color = "red")+
  geom_hline(yintercept = mean(chained$I, na.rm = T), color = "cadetblue3", size = 1.05)+
  geom_hline(yintercept = 0, lty = "dashed")+
  labs(title = "Chained Brazilian Macroeconomic Indicators plot",
       subtitle = paste0("Chained variables quarter on quarter from ",
                         I_macro$ano[1],
                         " to ",
                         I_macro$ano[nrow(I_macro)]),
       y = "% chained variance",
       x = "Tempo",
       caption = "Guilherme Viegas")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
