######################################################
## Código: Importando dados Google Trends           ##
##                Enrico Campos de Mira             ##
## Data:          18/09/2025                        ##
######################################################

library(gtrendsR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr) # Necessário para o complete() no final

# ========================================================= #
# 1. Configurações Iniciais
# ========================================================= #

keywords <- c("Bitcoin", "Ethereum", "Ripple", "Dogecoin", "Chainlink", 
              "Litecoin", "Tron", "Stellar", "Cardano", "Binance Coin")

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2026-06-19")

main_dir <- "gtrends_data"
dir.create(main_dir, showWarnings = FALSE)

create_windows <- function(start_date, end_date, window_size = 90) {
  seq_dates <- seq(start_date, end_date, by = paste(window_size - 1, "days"))
  end_dates <- seq_dates + window_size - 1
  end_dates[end_dates > end_date] <- end_date 
  
  windows <- data.frame(
    start = format(seq_dates, "%Y-%m-%d"),
    end = format(end_dates, "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  return(windows)
}

windows <- create_windows(start_date, end_date)

# ========================================================= #
# 2. Loop Mestre (1ª Tentativa de Coleta)
# ========================================================= #

for (kw in keywords) {
  cat("\n======================================================\n")
  cat("Iniciando coleta para:", kw, "\n")
  cat("======================================================\n")
  
  kw_safe_name <- gsub(" ", "_", kw)
  save_dir <- file.path(main_dir, paste0("gtrends_", kw_safe_name))
  dir.create(save_dir, showWarnings = FALSE)
  
  for (i in seq_len(nrow(windows))) {
    save_path <- file.path(save_dir, paste0("window_", i, ".rds"))
    
    if (file.exists(save_path)) {
      message(" -> Janela ", i, " de ", nrow(windows), " já coletada. Pulando.")
      next
    }
    
    cat(" -> Coletando janela", i, "de", nrow(windows), "(", windows$start[i], "a", windows$end[i], ")\n")
    time_string <- paste(windows$start[i], windows$end[i])
    
    tryCatch({
      res <- gtrends(
        keyword = kw,
        geo = "",  
        time = time_string,
        gprop = "web",
        onlyInterest = TRUE
      )$interest_over_time
      
      if (!is.null(res)) {
        saveRDS(res, file = save_path)
      } else {
        message(" -> Resultado nulo na janela ", i)
      }
      
      Sys.sleep(runif(1, min = 10, max = 15))  
      
    }, error = function(e) {
      message(" -> Erro na janela ", i, ": ", e$message)
    })
  }
}

# ========================================================= #
# 3. Auditoria, Validação e 2ª Tentativa (Retry Logic)
# ========================================================= #

cat("\n======================================================\n")
cat("Auditoria e Verificação de Integridade dos Dados...\n")
cat("======================================================\n")

# --- 3.1 O AUDITOR BLINDADO ---
# Abre todos os arquivos salvos e deleta os corrompidos ou com dados semanais
all_downloaded_files <- list.files(main_dir, pattern = "window_.*\\.rds$", recursive = TRUE, full.names = TRUE)
arquivos_deletados <- 0

for (arquivo in all_downloaded_files) {
  df <- tryCatch(readRDS(arquivo), error = function(e) NULL)
  deletar <- FALSE
  
  if (is.null(df) || nrow(df) < 70) {
    deletar <- TRUE # Arquivo vazio ou muito pequeno (bloqueio puro)
  } else if (!"date" %in% colnames(df)) {
    deletar <- TRUE # Estrutura corrompida
  } else {
    # Teste de espaçamento para caçar falsos positivos (dados semanais/mensais)
    df$date <- as.Date(df$date)
    espacamento_dias <- as.numeric(difftime(df$date[2], df$date[1], units = "days"))
    
    if (is.na(espacamento_dias) || espacamento_dias > 1) {
      deletar <- TRUE 
    }
  }
  
  if (deletar) {
    file.remove(arquivo)
    arquivos_deletados <- arquivos_deletados + 1
  }
}

cat(" -> Auditoria concluída. Arquivos falsos/corrompidos deletados:", arquivos_deletados, "\n\n")

# --- 3.2 MAPEAMENTO DO QUE FALTA ---
expected_tasks <- expand.grid(
  keyword = keywords,
  window_index = seq_len(nrow(windows)),
  stringsAsFactors = FALSE
) %>%
  left_join(windows %>% mutate(window_index = row_number()), by = "window_index") %>%
  mutate(
    safe_name = gsub(" ", "_", keyword),
    file_path = file.path(main_dir, paste0("gtrends_", safe_name), paste0("window_", window_index, ".rds"))
  )

missing_tasks <- expected_tasks %>% filter(!file.exists(file_path))

# --- 3.3 SEGUNDA TENTATIVA ---
if (nrow(missing_tasks) > 0) {
  cat("Atenção:", nrow(missing_tasks), "janela(s) pendente(s). Iniciando 2ª tentativa...\n")
  cat("Dica: Se o erro 429 persistir, reinicie seu modem/roteie o 4G para trocar o IP.\n\n")
  
  for (i in seq_len(nrow(missing_tasks))) {
    task <- missing_tasks[i, ]
    cat(" -> Retentando:", task$keyword, "- Janela", task$window_index, "(", task$start, "a", task$end, ")\n")
    
    tryCatch({
      res <- gtrends(
        keyword = task$keyword,
        geo = "",  
        time = paste(task$start, task$end),
        gprop = "web",
        onlyInterest = TRUE
      )$interest_over_time
      
      if (!is.null(res)) {
        saveRDS(res, file = task$file_path)
        cat("    [Sucesso! Arquivo salvo]\n")
      } else {
        cat("    [Falha: O Google não retornou dados]\n")
      }
      
      Sys.sleep(runif(1, min = 15, max = 20)) # Pausa maior na 2ª tentativa
      
    }, error = function(e) {
      cat("    [Erro persistente]:", e$message, "\n")
    })
  }
}

still_missing <- expected_tasks %>% filter(!file.exists(file_path))
if (nrow(still_missing) > 0) {
  cat("\nAVISO: Os seguintes dados continuam pendentes:\n")
  print(still_missing %>% dplyr::select(Moeda = keyword, Janela = window_index, Inicio = start, Fim = end))
} else {
  cat("\nSucesso Total! Nenhum buraco na base de dados.\n")
}

# ========================================================= #
# 4. Consolidação Geral e Visualização
# ========================================================= #

cat("\n======================================================\n")
cat("Unindo dados e gerando gráfico de painel...\n")
cat("======================================================\n")

all_files <- list.files(main_dir, pattern = "window_.*\\.rds$", recursive = TRUE, full.names = TRUE)

if (length(all_files) > 0) {
  data_list <- lapply(all_files, function(file) {
    df <- readRDS(file)
    df$hits <- as.character(df$hits) # Evita erro de integer vs character
    return(df)
  })
  
  master_data <- bind_rows(data_list) %>%
    dplyr::select(date, hits, keyword) %>%
    mutate(
      date = as.Date(date),
      hits = as.numeric(replace(hits, hits == "<1", "0"))
    ) %>%
    distinct(keyword, date, .keep_all = TRUE) %>% 
    # Preencher dias pendentes com NA para o gráfico ser honesto (se ainda houver buracos)
    group_by(keyword) %>%
    tidyr::complete(date = seq.Date(start_date, end_date, by = "day")) %>%
    ungroup() %>%
    arrange(keyword, date)
  
  saveRDS(master_data, file = file.path(main_dir, "gtrends_ALL_combined.rds"))
  
  p_trends <- ggplot(master_data, aes(x = date, y = hits)) +
    geom_line(color = "steelblue", size = 0.5) +
    facet_wrap(~ keyword, ncol = 2, scales = "free_y") + 
    labs(title = "Google Trends Diário (Atenção do Investidor)",
         x = " ", y = "Volume de Busca (0–100)") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_classic() +
    theme(
      strip.background = element_rect(fill = "gray90", color = "black"),
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    )
  
  print(p_trends)
} else {
  cat("Nenhum arquivo de dados encontrado para consolidar.\n")
}
