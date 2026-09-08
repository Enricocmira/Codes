############################################################
# Tabela Estatísticas Descritivas                          #
# Nomes: Enrico C. Mira                                    #
# Data 20/06/2026                                          #
############################################################

# Instale o pacote moments caso não o tenha
# install.packages("moments")

library(moments)

# 1. FUNÇÃO PARA GERAR A TABELA DESCRITIVA EM LATEX
generate_descriptive_table <- function(data) {
  
  # Cabeçalho da tabela
  cat("\\begin{table}[H]\n")
  cat("\\caption{Descriptive statistics of log-prices, log-returns, EWMA volatility, and log-volume of the selected cryptocurrencies over the period from November 09, 2017, to June 19, 2026.}\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat("\\begin{tabular}[t]{lrrrrrr}\n")
  cat("\\toprule\n")
  cat("Crypto &  Mean & Maximum & Minimum & Std. Dev. & Skewness & Kurtosis\\\\\n")
  cat("\\midrule\n")
  
  # Iterar pelas colunas do data.frame
  for (crypto in colnames(data)) {
    # Remove eventuais NAs para não quebrar o cálculo
    series <- na.omit(data[[crypto]])
    
    # Cálculos estatísticos
    mu <- mean(series)
    mx <- max(series)
    mn <- min(series)
    std <- sd(series)
    skw <- skewness(series)
    
    # Excesso de curtose (Subtrai 3, assumindo a distribuição normal como baseline 0)
    krt <- kurtosis(series) - 3 
    
    # Imprime a linha formatada (3 casas decimais para os 4 primeiros, 2 para os últimos)
    cat(sprintf("%-4s & %7.3f & %7.3f & %7.3f & %7.3f & %7.2f & %7.2f \\\\\n", 
                crypto, mu, mx, mn, std, skw, krt))
  }
  
  # Fechamento da tabela
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

# ==========================================
# 2. EXEMPLO DE USO (SIMULANDO DADOS)
# ==========================================

# Criando um dataset fictício de retornos simulados
set.seed(42)
n_obs <- 1000

df_crypto <- data.frame(
  ADA  = rnorm(n_obs, mean = -1.2, sd = 1.1),
  BNB  = rnorm(n_obs, mean = 4.8, sd = 1.7),
  BTC  = rnorm(n_obs, mean = 10.1, sd = 0.9),
  DOGE = rnorm(n_obs, mean = -3.4, sd = 1.8),
  ETH  = rnorm(n_obs, mean = 6.9, sd = 1.1),
  LINK = rnorm(n_obs, mean = 1.7, sd = 1.4),
  LTC  = rnorm(n_obs, mean = 4.4, sd = 0.4),
  TRX  = rnorm(n_obs, mean = -2.7, sd = 1.0),
  XLM  = rnorm(n_obs, mean = -1.8, sd = 0.6),
  XRP  = rnorm(n_obs, mean = -0.5, sd = 0.7)
)

# Gera o código LaTeX no console
generate_descriptive_table(df)
