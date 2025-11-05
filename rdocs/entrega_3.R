source("rdocs/source/packages.R")

library(readxl)
library(dplyr)
library(stringr)
library(tibble)

# 1. Ler as três planilhas que interessam
relatorio_vendas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
infos_clientes   <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_clientes")
infos_lojas      <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_lojas")

# 2. Filtrar apenas as lojas da cidade de Âmbar Seco
relatorio_vendas <- relatorio_vendas %>% filter(StoreID %in% c(2, 6, 8, 9))

# 3. Remover duplicatas de cliente por loja (um cliente aparece apenas 1x por loja)
relatorio_vendas <- relatorio_vendas %>%
  distinct(ClientID, StoreID, .keep_all = TRUE)

# 4. Juntar com as informações de clientes e lojas
relatorio_vendas <- relatorio_vendas %>%
  left_join(infos_clientes %>% select(ClientID, Age), by = "ClientID") %>%
  left_join(infos_lojas %>% select(StoreID, NameStore), by = "StoreID")

relatorio_vendas %>%
  group_by(StoreID) %>%
  print_quadro_resumo(var_name = Age)

boxplots <- ggplot(relatorio_vendas) +
  aes(x = reorder(NameStore, Age, FUN = median), y = Age) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(
    x = "Loja",
    y = "Idade dos clientes (anos)",
  ) +
  theme_estat()
ggsave("box_idade_por_loja.pdf", width = 158, height = 93, units = "mm")