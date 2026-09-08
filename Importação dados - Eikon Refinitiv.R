############################################################
# Eikon Refinitiv                                          #
# Nomes: Enrico C. Mira                                    #
# Data 20/06/2026                                          #
############################################################

# 1. Carregar as bibliotecas
library(eikonapir)
library(dplyr)
library(tidyr)

# 2. Autenticar no Eikon
eikonapir::set_app_id("MINHA-API")

# 3. Configurar a data final e a lista de ativos
data_hoje <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

ativos <- c(
  "BTC=BTSP",
  "ETH=BTSP",
  "ADA=BTSP",
  "LINK=BTSP",
  "TRX=BTSP",
  "XLM=BTSP",
  "LTC=BTSP",
  "DOGE=BTSP",
  "XRP=BTSP"
)

# 4. Fazer o loop iterando sobre a lista de ativos
lista_dados <- lapply(ativos, function(ticker) {
  
  # Usamos tryCatch para evitar que a API derrube o script caso um ativo falhe
  dados_ts <- tryCatch({
    get_timeseries(
      rics = ticker,                  
      start_date = "2017-01-01T00:00:00", 
      end_date = data_hoje,               
      interval = "daily",                 
      fields = list("TIMESTAMP", "CLOSE", "VOLUME")  
    )
  }, error = function(e) {
    return(NULL) # Se der erro, retorna nulo e não quebra o loop
  })
  
  # Se a API não retornar dados (tabela vazia ou nula), pula para a próxima cripto
  if (is.null(dados_ts) || nrow(dados_ts) == 0) return(NULL)
  
  # 5. Tratamento de Colunas e Datas à prova de falhas
  if (ncol(dados_ts) == 4) {
    
    # Cenário A: 4 colunas
    colnames(dados_ts) <- c("DATA", "CLOSE", "VOLUME", "TICKER")
    
    dados_limpos <- dados_ts %>%
      mutate(DATA = as.Date(substr(as.character(DATA), 1, 10)))
    
  } else {
    
    # Cenário B: 3 colunas e data no rownames
    colnames(dados_ts) <- c("CLOSE", "VOLUME", "TICKER")
    
    dados_limpos <- dados_ts %>%
      mutate(DATA = as.Date(substr(rownames(.), 1, 10))) %>%
      select(DATA, CLOSE, VOLUME, TICKER)
  }
  
  # Garante que a coluna TICKER seja formatada como texto (evita conflitos na hora de juntar)
  dados_limpos <- dados_limpos %>%
    mutate(TICKER = as.character(TICKER))
  
  return(dados_limpos)
})

# 6. Juntar tudo em uma única tabela contínua (Data Frame)
# O bind_rows ignora automaticamente os ativos que retornaram NULL no passo anterior
tabela_final_criptos <- bind_rows(lista_dados)

# Limpa o índice visual
rownames(tabela_final_criptos) <- NULL

# Exibe o resultado perfeitamente estruturado
print(head(tabela_final_criptos))

# 7. Transformar do formato LONG para o formato WIDE
# Isso vai criar colunas dinâmicas como: CLOSE_BTC=BTSP, VOLUME_BTC=BTSP, etc.
tabela_wide <- tabela_final_criptos %>%
  pivot_wider(
    names_from = TICKER, 
    values_from = c(CLOSE, VOLUME),
    names_sep = "_" # Define o separador do nome da coluna (Ex: CLOSE_BTC=BTSP)
  )

# 8. Exportar a tabela WIDE diretamente para um arquivo Excel
# O arquivo será salvo na sua pasta de trabalho atual (Working Directory) do R
writexl::write_xlsx(tabela_wide, path = "dados_cripto_wide.xlsx")

# 9. Mensagem de sucesso e visualização das primeiras linhas e colunas
print("Sucesso! O arquivo 'dados_cripto_wide.xlsx' foi salvo no seu computador.")
print(head(tabela_wide)) # Mostra a data e as primeiras colunas do formato WIDE
print(tail(tabela_wide)) # Mostra a data e as primeiras colunas do formato WIDE
