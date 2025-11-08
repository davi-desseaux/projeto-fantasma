library(dplyr)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(readxl)
library(tidytext)

# 1. Ler dados
vendas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
itens <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_vendas")
produtos <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_produtos")
lojas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_lojas")

# 2. Filtrar ano de 1889 e calcular receita
infos_lojas <- lojas %>% select(StoreID, NameStore)

vendas_1889 <- vendas %>%
  mutate(Ano = year(as.Date(Date))) %>%
  filter(Ano == 1889)

vendas_completa <- vendas_1889 %>%
  left_join(itens, by = "SaleID") %>%
  left_join(produtos, by = "ItemID") %>%
  left_join(infos_lojas, by = "StoreID") %>%
  mutate(
    UnityPrice = as.numeric(gsub(",", ".", UnityPrice)),
    Receita = Quantity * UnityPrice
  )

# 3. Top 3 lojas com maior receita
top3_lojas <- vendas_completa %>%
  group_by(StoreID, NameStore) %>%
  summarise(ReceitaTotal = sum(Receita, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(ReceitaTotal)) %>%
  slice_head(n = 3)

# 4. Top 3 produtos por loja (por quantidade vendida)
top3_produtos_por_loja <- vendas_completa %>%
  filter(StoreID %in% top3_lojas$StoreID) %>%
  group_by(NameStore, NameProduct) %>%
  summarise(
    QuantidadeVendida = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(NameStore, desc(QuantidadeVendida)) %>%
  group_by(NameStore) %>%
  slice_head(n = 3) %>%
  ungroup()

# 5. Calcular frequências relativas dentro da loja
produtos_freq <- top3_produtos_por_loja %>%
  left_join(
    vendas_completa %>%
      group_by(NameStore) %>%
      summarise(TotalQtdLoja = sum(Quantity, na.rm = TRUE)),
    by = "NameStore"
  ) %>%
  mutate(
    freq_relativa = round(QuantidadeVendida / TotalQtdLoja * 100, 1)
  )

# Criar colunas de texto (legendas)
porcentagens <- str_replace(str_c(produtos_freq$freq_relativa, "%"), "\\.", ",")
legendas <- str_squish(str_c(produtos_freq$QuantidadeVendida, " (", porcentagens, ")"))
produtos_freq <- produtos_freq %>%
  mutate(legendas = legendas)

# 6. Gráfico bivariado (colunas agrupadas)
grafico_bivariado <- ggplot(produtos_freq) +
  aes(
    x = fct_reorder(NameStore, TotalQtdLoja, .desc = TRUE),  # eixo x = loja
    y = QuantidadeVendida,
    fill = NameProduct,  # cor = produto
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(
    x = "Loja",
    y = "Quantidade vendida",
    title = "Top 3 produtos das 3 lojas com maior receita em 1889",
    fill = "Produto"
  ) +
  theme_estat()

ggsave("colunas-bivariadas-top3-produtos-por-loja.pdf",
       width = 158, height = 93, units = "mm")
