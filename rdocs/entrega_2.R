source("rdocs/source/packages.R")

library(readxl)
library(dplyr)
library(lubridate)
library(dplyr)
library(ggplot2)

##Análise 2
infos_clientes <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_clientes")

# Renomear e converter as variáveis para o sistema internacional
infos_clientes <- infos_clientes |> rename(peso = Weight_lbs, altura = Height_dm) |> mutate(peso = peso * 0.45359237, altura = altura * 10)

# Arrumando as alturas
infos_clientes <- infos_clientes |> mutate(altura = round(altura, 2))

# Criando as variáveis de interesse
peso <- infos_clientes$peso
altura <- infos_clientes$altura

# Verificando normalidade
shapiro.test(peso)
shapiro.test(altura)
# Como não existem existem evidências estatísticas suficientes para concluir que os dados seguem uma distribuição Normal, seguimos para testes não paramétricos

# TESTE DE SPEARMAN
cor.test(peso, altura, method = "spearman")
# Como o p-valor é muito menor que 0.05, há evidência estatística de correlação positiva (se um varia, o outro tende a variar no mesmo sentido) entre altura e peso.
# Mesmo considerando os empates, a correlação estimada é forte (ρ≈0.69) e o p-valor indica que essa relação não é coincidência.

grafico_2 <- ggplot(infos_clientes) +
  aes(x = altura, y = peso) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.6) +
  labs(
    x = "Altura (em cm)",
    y = "Peso (em kg)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
print(grafico_2)

