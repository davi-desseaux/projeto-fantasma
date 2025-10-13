source("rdocs/source/packages.R")

library(readxl)
library(dplyr)
library(lubridate)

##Análise 1

# Importando os dados da primeira entrega - talvez não o jeito mais otimizado, olhar depois
relatorio_vendas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx",sheet = "relatorio_vendas")
infos_vendas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx",sheet = "infos_vendas")
infos_produtos <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx",sheet = "infos_produtos")

# 1. Juntar tudo em um único dataframe
df_completo <- relatorio_vendas %>%
  left_join(infos_vendas, by = "SaleID") %>%
  left_join(infos_produtos, by = "ItemID") %>%
  mutate(
    UnityPrice = as.numeric(UnityPrice),
    Quantity = as.numeric(Quantity),
    Receita_USD = UnityPrice * Quantity,
    Taxa_Cambio = 5.31,
    Receita_BRL = Receita_USD * Taxa_Cambio,
    Ano = year(Date)
  ) %>%
  
  filter(!is.na(Receita_BRL))

# 2. Filtrar o período de interesse (1880 a 1889)
df_decada <- df_completo %>%
  filter(Ano >= 1880 & Ano <= 1889)

# 3. Calcular as métricas
relatorio_anual <- df_decada %>%
  group_by(Ano, StoreID) %>%
  summarise(Receita_Total_Loja_BRL = sum(Receita_BRL, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Ano) %>%
  summarise(Receita_Media_Lojas_BRL = mean(Receita_Total_Loja_BRL, na.rm = TRUE)) %>%
  ungroup()

# Gerar gráfico
grafico_1 <- ggplot(relatorio_anual) +
  aes(x = Ano, y = Receita_Media_Lojas_BRL, group=1) +
  geom_line(linewidth=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Receita média das lojas (em reais)") +
  scale_x_continuous(
    breaks = 1880:1889,           # força todos os anos no eixo x
    labels = 1880:1889            # garante que apareça sem vírgula ou decimal
  ) +
  theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")
