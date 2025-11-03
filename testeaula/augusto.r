############################ README — augusto.r
#
# Resumo rápido
# ------------
# Este script lê listas em PDFs e realiza identificação com base em
# características marcadas (números da lista). Fornece funções para
# ler PDFs, extrair números marcados (itens com "X") e identificar o
# melhor candidato segundo regras predefinidas.
#
# Requisitos
# ----------
# - R (recomendado >= 3.5)
# - Pacote `pdftools` (para leitura de PDFs). Instale antes de rodar:
#   install.packages("pdftools", repos = "https://cran.rstudio.com")
#
# Uso rápido
# ---------
# 1) Source o script:
#    source('c:/Users/USER/Documents/Mestrado/Análises/aula_pens.r')
# 2) Processar PDFs:
#    res <- processar_pdfs(c('C:/.../listac.pdf','C:/.../outro.pdf'), per_page = FALSE)
#
# Observações
# - Se os PDFs forem imagens (scans), será necessário OCR (ex.: tesseract).
###################
  ## FUNÇÃO UNIFICADA: ler_pdf
  ## Lê um PDF a partir de um caminho (string) ou de um raw vector com dados PDF.
  ## Retorna um vetor de strings (páginas) ou lança erro informativo.
  ler_pdf <- function(pdf_input, install_if_missing = FALSE, verbose = FALSE){
    if(missing(pdf_input) || (is.character(pdf_input) && !nzchar(pdf_input))){
      stop("Forneça o caminho para o PDF em 'pdf_input' ou um raw vector com os dados do PDF.")
    }

    # garantir pdftools (instalar opcionalmente)
    if(!requireNamespace("pdftools", quietly = TRUE)){
      if(install_if_missing){
        if(verbose) message("Pacote 'pdftools' não encontrado — instalando...")
        install.packages("pdftools", repos = "https://cran.rstudio.com")
        if(!requireNamespace("pdftools", quietly = TRUE)) stop("Falha ao instalar 'pdftools'. Instale manualmente e tente novamente.")
      } else {
        stop("Pacote 'pdftools' não encontrado. Use install_if_missing = TRUE para instalar automaticamente.")
      }
    }

    # Se forneceram raw vector com dados do PDF
    if(is.raw(pdf_input)){
      if(verbose) message('Lendo PDF a partir de raw vector')
      return(tryCatch(pdftools::pdf_text(pdf_input), error = function(e) stop(sprintf('Erro ao ler PDF a partir de raw: %s', e$message))))
    }

    # Se for string, limpar prefixo file:// e normalizar caminho
    if(is.character(pdf_input) && length(pdf_input) == 1){
      path <- sub('^file://+','', as.character(pdf_input))
      path_norm <- tryCatch(normalizePath(path, winslash = '/', mustWork = FALSE), error = function(e) path)
      if(!file.exists(path_norm)) stop(sprintf("Arquivo PDF não encontrado: '%s' (avaliado como '%s'). Verifique o caminho.", pdf_input, path_norm))
      if(verbose) message('Lendo PDF: ', path_norm)
      return(tryCatch(pdftools::pdf_text(path_norm), error = function(e) stop(sprintf('Erro ao ler PDF "%s": %s', path_norm, e$message))))
    }

    stop("Argumento 'pdf_input' deve ser uma string (caminho) ou um raw vector com dados PDF.")
  }

  ## extrair_numeros: extrai números marcados a partir de texto (páginas ou string única)
  ## Aceita um vetor de páginas (character) ou uma única string com o texto concatenado.
  extrair_numeros <- function(text_or_pages){
    if(missing(text_or_pages) || length(text_or_pages) == 0) return(numeric(0))
    # se for vetor de páginas, juntar; se for único texto, usar direto
    if(length(text_or_pages) > 1) texto <- paste(text_or_pages, collapse = "\n") else texto <- as.character(text_or_pages)
    linhas <- unlist(strsplit(texto, "\n"))

    pattern <- "(?i)PRESENTE\\s*[:\\-]?\\s*X"
    marcadas <- linhas[grepl(pattern, linhas, perl = TRUE)]
    if(length(marcadas) == 0){
      has_presente <- grepl("(?i)PRESENTE", linhas, perl = TRUE)
      has_x <- grepl("(?i)\\bX\\b", linhas, perl = TRUE)
      marcadas <- linhas[has_presente & has_x]
    }

    if(length(marcadas) == 0) return(numeric(0))

    numeros_start <- regmatches(marcadas, regexpr("^\\s*\\d+", marcadas, perl = TRUE))
    if(length(numeros_start) == 0 || all(nchar(numeros_start) == 0)){
      numeros_any <- regmatches(marcadas, regexpr("\\d+", marcadas, perl = TRUE))
      numeros <- numeros_any
    } else {
      numeros <- numeros_start
    }

    numeros <- as.numeric(numeros)
    numeros <- sort(unique(numeros[!is.na(numeros)]))
    return(numeros)
  }
  # Exemplo de uso
  # extrair_numeros("C:/Users/USER/Downloads/listac.pdf")

  ## processar_pdfs: fluxo único para ler, extrair e identificar
  processar_pdfs <- function(paths, per_page = FALSE, verbose = FALSE, install_if_missing = FALSE){
    if(missing(paths) || length(paths) == 0) stop("Forneça ao menos um caminho de PDF em 'paths'.")
    if(!is.character(paths)) stop("'paths' deve ser um vetor de caracteres com caminhos para arquivos PDF.")
    results <- list()

    for(i in seq_along(paths)){
      p <- paths[i]
      if(verbose) message(sprintf("Processando [%d/%d]: %s", i, length(paths), p))

      pages <- tryCatch(ler_pdf(p, install_if_missing = install_if_missing, verbose = verbose), error = function(e) e)
      if(inherits(pages, "error")){
        results[[length(results) + 1]] <- list(path = p, error = pages$message)
        next
      }

      if(per_page){
        for(pg in seq_along(pages)){
          texto_pg <- pages[pg]
          nums <- extrair_numeros(texto_pg)
          id <- if(length(nums)==0) list(resultado = "Sem características detectadas", melhor_candidato = NA, confianca = 0) else identificar(nums, verbose = verbose)
          results[[length(results) + 1]] <- list(path = p, page = pg, numeros = nums, identificacao = id)
        }
      } else {
        nums <- extrair_numeros(pages)
        id <- if(length(nums)==0) list(resultado = "Sem características detectadas", melhor_candidato = NA, confianca = 0) else identificar(nums, verbose = verbose)
        results[[length(results) + 1]] <- list(path = p, page = NA, numeros = nums, identificacao = id)
      }
    }

    return(results)
  }

  # Exemplo de uso:
  # source('c:/Users/USER/Documents/Mestrado/Análises/aula_pens.r')
  # res <- processar_pdfs(c('C:/.../listac.pdf','C:/.../outro.pdf'), per_page = FALSE, verbose = TRUE, install_if_missing = FALSE)

  # Observação: o arquivo foi simplificado para quatro funções principais:
  # - ler_pdf(pdf_input, ...)
  # - extrair_numeros(text_or_pages)
  # - identificar(obs)
  # - processar_pdfs(paths, ...)
  # Chamadas auxiliares foram removidas para manter a API enxuta.
## ============================================================
## LISTA DE CARACTERÍSTICAS
## ============================================================

caracteristicas <- list(
  `1`="Seis pares de patas",
  `2`="Patas foliáceas",
  `3`="Quatro ou cinco pares de patas",
  `6`="Algumas patas preênseis e outras foliáceas",
  `7`="Antena da fêmea com um ramo",
  `8`="Ramo cilíndrico e com três setas terminais",
  `9`="Cabeça claramente delimitada, comprimento muito superior à largura",
  `10`="Antena da fêmea com dois ramos",
  `11`="Ramos achatados com numerosas setas, terminais ou laterais",
  `12`="Cabeça não claramente delimitada, com comprimento pouco excedendo a largura",
  `13`="Fêmea com antênulas fixas, fundidas com o rostro",
  `14`="Fêmea com antênulas livres, não fundidas com o rostro",
  `15`="Rostro e fórnices unidos, recobrindo as antênulas",
  `16`="Ramos das antenas com 3 segmentos",
  `17`="Rostro e fórnices não unidos",
  `18`="Ramos da antena com 4 segmentos",
  `19`="Antênulas da fêmea pequenas e imóvelmente presas à cabeça",
  `22`="Antênulas móveis",
  `24`="Antênulas bi-segmentadas",
  `25`="Antênula com um segmento",
  `26`="Antênulas atadas à margem ventral-anterior da cabeça",
  `27`="Posabdômen com porção pós-anal não cônica",
  `28`="Antênulas atadas à margem ventral-posterior da cabeça",
  `29`="Posabdômen com porção pós-anal cônica"
)


## ============================================================
## REGRAS DE IDENTIFICAÇÃO 
## ============================================================

regras <- list(
  Ctenopoda = c(1,2),
  Ctenopoda_Holopedidae = c(1,2,7,8,9),
  Ctenopoda_Sididae = c(1,2,11,12),

  Anomopoda = c(3,6),
  Anomopoda_Bosminidae = c(3,6,13),
  Anomopoda_Chydoridae = c(3,6,14,15,16),
  Anomopoda_Daphniidae_A = c(3,6,14,17,16,19),
  Anomopoda_Daphniidae_B = c(3,6,14,17,18,19),

  Anomopoda_Ilycryptidae_A = c(3,6,14,17,18,22,24),
  Anomopoda_Ilycryptidae_B = c(3,6,14,17,16,22),

  Anomopoda_Macrothricidae_A = c(3,6,14,17,18,22,25,26,27),
  Anomopoda_Macrothricidae_B = c(3,6,14,17,16,22,25,26,27),

  Anomopoda_Moinidae_A = c(3,6,14,17,18,22,25,28,29),
  Anomopoda_Moinidae_B = c(3,6,14,17,16,22,25,28,29)
)


## ============================================================
## FUNÇÃO DE IDENTIFICAÇÃO
## ============================================================

identificar <- function(obs, verbose = FALSE){
  # Validação básica da entrada
  if(missing(obs)) stop("Forneça um vetor de características (ex.: c(1,6,13)).")

  # Aceitar também uma única string com números separados por vírgula/espaco: "1,2" ou "1 2"
  if(length(obs) == 1 && is.character(obs)){
    s_clean <- gsub("[^0-9, ]", "", obs)
    obs_num <- as.numeric(unlist(strsplit(s_clean, "[, ]+")))
    obs_num <- sort(unique(obs_num[!is.na(obs_num)]))
  } else {
    obs_num <- suppressWarnings(as.numeric(obs))
    obs_num <- sort(unique(obs_num[!is.na(obs_num)]))
  }

  if(length(obs_num) == 0){
    return(list(
      resultado = "Entrada inválida",
      interpretacao = "Nenhuma característica numérica válida fornecida (NAs removidos).",
      confianca = 0
    ))
  }


  # Garantir que o objeto regras existe
  if(!exists("regras") || !is.list(regras)){
    stop("Objeto 'regras' não encontrado no ambiente. Sourceie o arquivo que define 'regras'.")
  }

  # 1) identificar regras que são totalmente cobertas por obs
  fully_matched_names <- names(regras)[sapply(regras, function(r) all(r %in% obs_num))]
  if(verbose) message("Observações (num): ", paste(obs_num, collapse=","), " | Fully matched: ", paste(fully_matched_names, collapse=","))

  if(length(fully_matched_names) > 0){
    # escolher a regra mais específica (a que tem mais caracteres na definição)
    lengths_matched <- sapply(regras[fully_matched_names], length)
    chosen_name <- fully_matched_names[which.max(lengths_matched)]
    chosen_rule <- regras[[chosen_name]]
    extras <- setdiff(obs_num, chosen_rule)
    if(length(extras) > 0){
      return(list(
        resultado = paste("Identificação provável:", chosen_name),
        confianca = 1,
        aviso = "Atenção: características adicionais foram marcadas. Pode indicar erro de observação, variação ou espécie próxima.",
        caracteristicas_extras = extras
      ))
    } else {
      return(list(
        resultado = paste("Identificação exata:", chosen_name),
        confianca = 1
      ))
    }
  }

  # 2) se nenhuma regra foi totalmente coberta, calcular similaridades
  scores <- sapply(regras, function(regra) {
    length(intersect(obs_num, regra)) / length(regra)
  })

  melhor <- names(scores)[which.max(scores)]
  confianca <- max(scores)

  # 3) se maior similaridade >= 0.60 -> retornar top 3 (com valores)
  if(confianca >= 0.60){
    top3 <- sort(scores, decreasing = TRUE)[1:min(3, length(scores))]
    return(list(
      resultado = "Nenhuma identificação exata, mas há candidatos fortes",
      interpretacao = "Observações parcialmente compatíveis. Forneça mais características para aumentar confiança.",
      top3 = top3,
      melhor_candidato = melhor,
      confianca = as.numeric(confianca)
    ))
  }

  # Caso a maior similaridade seja menor que 0.60, retornamos o melhor candidato
  # mas indicando baixa confiança
  return(list(
    resultado = "Nenhuma identificação confiável",
    interpretacao = "Sem regras suficientemente próximas.",
    melhor_candidato = melhor,
    confianca = as.numeric(confianca)
  ))
}

## Para chamadas simples, use diretamente identificar("1,2") ou identificar(c(1,2)).
