library(dplyr)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(readxl)
library(tidytext) # Necessário para ordenar dentro dos facets

# ------------------------------------------------------------
# 1. Ler dados
# ------------------------------------------------------------
vendas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
itens <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_vendas")
produtos <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_produtos")
lojas <- relatorio_old_town_road <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_lojas")

# ------------------------------------------------------------
# 2. Filtrar ano de 1889 e calcular receita
# ------------------------------------------------------------
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
# ------------------------------------------------------------
# 3. Selecionar as 3 lojas com maior receita total em 1889
# ------------------------------------------------------------
top3_lojas <- vendas_completa %>%
  group_by(StoreID) %>%
  summarise(ReceitaTotal = sum(Receita, na.rm = TRUE)) %>%
  arrange(desc(ReceitaTotal)) %>%
  slice_head(n = 3)

# ------------------------------------------------------------
# 4. Calcular top 3 produtos por loja (quantidade vendida)
# ------------------------------------------------------------
top3_produtos_por_loja <- vendas_completa %>%
  filter(StoreID %in% top3_lojas$StoreID) %>%
  group_by(NameStore, NameProduct) %>% # ALTERADO: Agrupamento por NameStore
  summarise(
    QuantidadeVendida = sum(Quantity, na.rm = TRUE),
    Receita = sum(Receita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(NameStore, desc(QuantidadeVendida)) %>%
  group_by(NameStore) %>% # ALTERADO: Agrupamento por NameStore
  slice_head(n = 3) %>%
  ungroup()

# ------------------------------------------------------------
# 5. Definir a ordem das lojas
# ------------------------------------------------------------
ordem_lojas <- vendas_completa %>%
  filter(StoreID %in% top3_lojas$StoreID) %>%
  group_by(NameStore) %>% # ALTERADO: Agrupamento por NameStore
  summarise(ReceitaTotal = sum(Receita, na.rm = TRUE)) %>%
  arrange(desc(ReceitaTotal)) %>%
  pull(NameStore) # ALTERADO: Puxar o NameStore

# ------------------------------------------------------------
# 6. Calcular porcentagens em relação ao total da loja
# ------------------------------------------------------------
produtos_freq <- top3_produtos_por_loja %>%
  left_join(
    vendas_completa %>%
      group_by(NameStore) %>% # ALTERADO: Agrupamento por NameStore
      summarise(TotalQtdLoja = sum(Quantity, na.rm = TRUE)),
    by = "NameStore" # ALTERADO: Junção por NameStore
  ) %>%
  mutate(
    freq_relativa_loja = round(QuantidadeVendida / TotalQtdLoja * 100, 1),
    porcentagens = str_replace(str_c(freq_relativa_loja, "%"), "\\.", ","),
    legendas = str_c(QuantidadeVendida, " (", porcentagens, ")"),
    NameStore = factor(NameStore, levels = ordem_lojas) # ALTERADO: Fator para NameStore
  )
# ------------------------------------------------------------
# 7. Criar coluna de fator com produtos ordenados por quantidade dentro de cada loja
# ------------------------------------------------------------
produtos_freq <- produtos_freq %>%
  group_by(NameStore) %>%
  mutate(
    # reorder_within para ordenar por loja
    NameProduct_ord = reorder_within(NameProduct, QuantidadeVendida, NameStore), 
    # fct_rev para inverter a ordem, garantindo MAIOR para o MENOR visualmente
    NameProduct_ord = fct_rev(NameProduct_ord) 
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 8. Gerar o gráfico facetado
# ------------------------------------------------------------
grafico_top3_produtos <- ggplot(produtos_freq) +
  aes(
    x = NameProduct_ord,
    y = QuantidadeVendida,
    fill = NameStore, # Fill por Nome da Loja
    label = legendas
  ) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(vjust = -0.3, size = 3) +
  facet_wrap(~ NameStore, scales = "free_x") + # ALTERADO: Facet por NameStore!
  labs(
    x = "Produto",
    y = "Quantidade vendida",
    title = "Top 3 produtos das 3 lojas com maior receita em 1889"
  ) +
  theme_estat() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  ) +
  scale_x_reordered()
ggsave("colunas-top3-produtos-por-loja-ordenado-por-nome.pdf",width = 158, height = 93, units = "mm")
