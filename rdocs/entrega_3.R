source("rdocs/source/packages.R")

library(readxl)
library(dplyr)
library(stringr)
library(tibble)

relatorio_vendas <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
infos_clientes   <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_clientes")
infos_lojas      <- read_excel("~/Área de Trabalho/PS ESTAT/projeto fantasma/relatorio_old_town_road.xlsx", sheet = "infos_lojas")

relatorio_vendas <- relatorio_vendas %>% filter(StoreID %in% c(2, 6, 8, 9))

relatorio_vendas <- relatorio_vendas %>% 
  left_join(infos_clientes %>% select(ClientID, Age), by = "ClientID") %>%
  left_join(infos_lojas %>% select(StoreID, NameStore), by = "StoreID")

print_quadro_resumo <- function(data, var_name, 
                                title = "Medidas resumo da(o) [nome da variável]", 
                                label = "quad:quadro_resumo1") {
  
  var_name <- substitute(var_name)
  
  data <- data %>%
    summarize(
      `Média` = round(mean(!!sym(var_name)), 2),
      `Desvio Padrão` = round(sd(!!sym(var_name)), 2),
      `Variância` = round(var(!!sym(var_name)), 2),
      `Mínimo` = round(min(!!sym(var_name)), 2),
      `1º Quartil` = round(quantile(!!sym(var_name), probs = .25), 2),
      `Mediana` = round(quantile(!!sym(var_name), probs = .5), 2),
      `3º Quartil` = round(quantile(!!sym(var_name), probs = .75), 2),
      `Máximo` = round(max(!!sym(var_name)), 2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c(
    "\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{|l|S[table-format=3.2]|}
\t\\toprule
\t\\textbf{Estatística} & \\textbf{Valor}\\\\
\t\\midrule\n"
  )
  
  for (i in seq_len(nrow(data))) {
    latex <- str_c(latex,
                   "\t", data[i, 1], " & ", data[i, 2], "\\\\\n")
  }
  
  latex <- str_c(
    latex,
    "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}"
  )
  
  writeLines(latex)
}

for (loja in unique(relatorio_vendas$NameStore)) {
  relatorio_vendas %>%
    filter(NameStore == loja) %>%
    print_quadro_resumo(
      var_name = Age,
      title = paste0("Medidas resumo da idade dos clientes — ", loja),
      label = paste0("quad:idade_", str_replace_all(loja, " ", "_"))
    )
}

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


