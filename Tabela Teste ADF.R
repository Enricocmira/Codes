############################################################
# Tabela Teste ADF                                         #
# Nomes: Enrico C. Mira                                    #
# Data 20/06/2026                                          #
############################################################

library(CADFtest)

# 1. FUNÇÃO PARA RODAR O ADF (MAIC) E FORMATAR O RESULTADO
get_adf_stats <- function(series, type) {
  # type aceita: "none", "drift", "trend"
  
  # Roda o teste ADF usando o critério MAIC para seleção de defasagens
  test <- CADFtest(series, type = type, criterion = "MAIC", max.lag.y = 5)
  
  stat <- test$statistic
  pval <- test$p.value
  lags <- test$max.lag.y
  
  # Definindo as estrelas de significância
  stars <- ""
  if (pval < 0.01) {
    stars <- "^{***}"
  } else if (pval < 0.05) {
    stars <- "^{**}"
  } else if (pval < 0.10) {
    stars <- "^{*}"
  }
  
  # Retorna a string formatada para o LaTeX
  return(sprintf("$%.3f%s$ & %d", stat, stars, lags))
}

# 2. FUNÇÃO PRINCIPAL PARA GERAR A TABELA LATEX (SEM PAINÉIS)
generate_adf_latex_table <- function(data, variables) {
  
  # Cabeçalho do LaTeX
  cat("\\section{Unit Root Tests\\label{ap:adf}}\n\n")
  cat("\\begin{table}[H]\n")
  cat("\\renewcommand{\\thetable}{A.1}\n")
  cat("\\caption{\\textbf{ADF Test Statistics for Investor Behavior and Control Variables}. }\n")
  cat("\\centering\n")
  cat("\\footnotesize\n")
  cat("\\label{tab:unit_roots}\n")
  cat("\\begin{tabular}{lcccccc}\n")
  cat("\\toprule\n")
  cat(" & \\multicolumn{2}{c}{ADF (none)} & \\multicolumn{2}{c}{ADF (constant)} & \\multicolumn{2}{c}{ADF (constant and trend)} \\\\\n")
  cat("\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}\n")
  cat("Variable & Stat & Lags & Stat & Lags & Stat & Lags \\\\\n")
  cat("\\midrule\n")
  
  # Loop contínuo através de todas as variáveis da lista
  for (var in variables) {
    # Verifica se a variável existe nos dados
    if (var %in% colnames(data)) {
      series <- data[[var]]
      
      # Calcula os 3 modelos ADF
      res_none  <- get_adf_stats(series, "none")
      res_const <- get_adf_stats(series, "drift")
      res_trend <- get_adf_stats(series, "trend")
      
      # Imprime a linha da variável
      cat(sprintf("%s & %s & %s & %s \\\\\n", var, res_none, res_const, res_trend))
    } else {
      warning(sprintf("Variável '%s' não encontrada no data.frame.", var))
    }
  }
  
  # Rodapé da Tabela LaTeX
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\begin{flushleft}\n")
  cat("\\footnotesize\n")
  cat("\\justifying\n")
  cat("\\emph{Notes:} This table reports the Augmented Dickey–Fuller (ADF) test statistics for the investor behavior measures and control variables (trading volume and volatility) employed in the bubble identification models. The ADF tests are conducted under specifications with no constant and trend (none), with constant and no trend (constant) and including both constant and trend (constant and trend). Lag lengths are selected according to the Modified Akaike Information Criterion (MAIC).\\\\\n")
  cat("(***), (**), (*) denote statistical significance at the 1\\%, 5\\%, and 10\\% levels, respectively.\n")
  cat("\\end{flushleft}\n")
  cat("\\end{table}\n")
}

# ==========================================
# 3. EXEMPLO DE USO (SIMULANDO DADOS)
# ==========================================

set.seed(123)
n_obs <- 200
df <- data.frame(
  Positive  = rnorm(n_obs),
  Negative  = rnorm(n_obs),
  Optimism  = rnorm(n_obs),
  Pessimism = rnorm(n_obs),
  PriceUp   = rnorm(n_obs),
  PriceDown = rnorm(n_obs),
  Momentum  = rnorm(n_obs),
  Volume    = rnorm(n_obs),
  Volatility= rnorm(n_obs)
)

# Agora basta passar um vetor simples com as variáveis na ordem desejada
my_variables <- c("Positive", "Negative", "Optimism", "Pessimism", 
                  "PriceUp", "PriceDown", "Momentum", "Volume", "Volatility")

# Gera o código LaTeX no console
generate_adf_latex_table(data = df, variables = my_variables)
