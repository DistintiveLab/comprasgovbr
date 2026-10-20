#' @title Infraestrutura interna de chamadas à API do Compras.gov.br
#' @description Wrapper interno que captura erros de conexão e emite aviso
#'   em vez de abortar a execução, seguindo o padrão adotado no pacote
#'   \code{sidra}.
#'
#'   O import explícito de \code{data.table} é indispensável: sem ele o
#'   namespace do pacote não é reconhecido como "data.table-aware" pela
#'   heuristic cedta() do data.table e todas as operações \code{[} passam a
#'   usar a semântica de data.frame.
#'
#' @import data.table
#' @param expr Expressão a avaliar (ex.: uma chamada \code{httr::GET()}).
#' @return O resultado da expressão, ou \code{NULL} invisível em caso de erro
#'   de conexão.
#' @noRd
call_compras <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      if (grepl(
        "Timeout was reached|Could not connect|Connection timed out|Could not resolve host|could not be reached|SSL",
        e$message
      )) {
        warning(
          "A API de Dados Abertos do Compras.gov.br nao pode ser alcancada.\n",
          "Verifique sua conexao com a internet e o acesso a dadosabertos.compras.gov.br, ou tente mais tarde.",
          call. = FALSE
        )
        return(invisible(NULL))
      } else {
        warning(e$message, call. = FALSE)
        return(invisible(NULL))
      }
    }
  )
}

#' URL base da API
#' @noRd
.compras_base_url <- function() {
  "https://dadosabertos.compras.gov.br"
}

#' Prepara a query string: descarta nulos/NA e normaliza logicais e datas
#' @noRd
.compras_query <- function(...) {
  q <- list(...)
  if (!length(q)) return(NULL)
  q <- q[!vapply(q, function(x) is.null(x) || (length(x) == 1 && is.na(x)), logical(1))]
  if (!length(q)) return(NULL)
  lapply(q, function(x) {
    if (is.logical(x)) {
      tolower(as.character(x))
    } else if (inherits(x, "Date")) {
      format(x, "%Y-%m-%d")
    } else {
      as.character(x)
    }
  })
}

#' Executa um GET único na API e devolve a resposta httr processada
#' @details O envelope de resposta padrao da API contem \code{resultado},
#'   \code{totalRegistros}, \code{totalPaginas} e \code{paginasRestantes}.
#'   Erros HTTP sao convertidos em condicao informativa, incluindo o corpo
#'   de validacao retornado pela API.
#' @noRd
.compras_get <- function(caminho, query = NULL) {
  url <- paste0(.compras_base_url(), caminho)
  resposta <- call_compras({
    httr::GET(
      url,
      query = query,
      httr::user_agent("comprasgovbr (https://github.com/distintivelab/comprasgovbr)"),
      httr::timeout(300)
    )
  })
  if (is.null(resposta)) {
    return(invisible(NULL))
  }
  status <- httr::status_code(resposta)
  if (status >= 400) {
    corpo <- httr::content(resposta, "text", encoding = "UTF-8", as = "text")
    detalhe <- ""
    if (nchar(trimws(corpo)) > 0) {
      detalhe <- tryCatch({
        err <- jsonlite::fromJSON(corpo, simplifyVector = FALSE)
        msg <- err$detail %||% err$message
        if (!is.null(msg)) paste0(" - ", msg) else ""
      }, error = function(e) "")
    }
    stop(
      "Falha na consulta (HTTP ", status, ") a '", caminho, "'", detalhe,
      call. = FALSE
    )
  }
  resposta
}

#' Extrai o conteudo JSON da resposta
#' @noRd
.compras_json <- function(resposta) {
  txt <- httr::content(resposta, "text", encoding = "UTF-8", as = "text")
  jsonlite::fromJSON(txt, simplifyDataFrame = TRUE, flatten = TRUE)
}

#' Concatena listas de registros em um data.table
#' @noRd
.compras_rbind <- function(resultado) {
  if (!length(resultado)) {
    return(data.table::data.table())
  }
  dt <- data.table::as.data.table(resultado)
  if (!nrow(dt)) dt else data.table::setDT(dt)[]
}

#' Percorre todas as paginas de um endpoint automaticamente
#' @param caminho Caminho relativo do endpoint (ex.:
#'   \code{"/modulo-uasg/1_consultarUasg"}).
#' @param query Lista nomeada com os parametros de consulta.
#' @param paginar Se \code{TRUE} (padrao), percorre todas as paginas; se
#'   \code{FALSE}, retorna apenas a pagina indicada.
#' @param tamanho_pagina Tamanho da pagina. A maioria dos endpoints exige
#'   minimo de 10 registros por pagina; o padrao da API e 500.
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com os registros de \code{resultado}.
#' @noRd
.compras_get_paginas <- function(caminho, query = NULL, paginar = TRUE,
                                 tamanho_pagina = NULL, quieto = FALSE) {
  query <- query %||% list()
  if (!is.null(tamanho_pagina)) {
    if (tamanho_pagina < 10 && tamanho_pagina >= 0) {
      stop("tamanho_pagina deve ser no minimo 10 (validacao da API).", call. = FALSE)
    }
    query$tamanhoPagina <- as.integer(tamanho_pagina)
  }
  if (is.null(query$pagina)) query$pagina <- 1L

  resposta <- .compras_get(caminho, query)
  if (is.null(resposta)) return(invisible(NULL))
  conteudo <- .compras_json(resposta)

  if (!is.list(conteudo) || is.null(conteudo$resultado)) {
    if (is.data.frame(conteudo)) return(.compras_rbind(conteudo))
    stop("Resposta inesperada da API para '", caminho, "'.", call. = FALSE)
  }

  registros <- .compras_rbind(conteudo$resultado)
  if (!paginar || is.null(conteudo$paginasRestantes)) {
    attr(registros, "totalRegistros") <- conteudo$totalRegistros
    attr(registros, "totalPaginas") <- conteudo$totalPaginas
    return(registros)
  }

  total_paginas <- conteudo$totalPaginas
  if (isTRUE(total_paginas > 1) && !quieto) {
    message("Consulta com ", total_paginas, " paginas (",
            conteudo$totalRegistros, " registros). Buscando...")
  }
  todas <- list(registros)
  while (isTRUE(conteudo$paginasRestantes > 0)) {
    query$pagina <- query$pagina + 1L
    resposta <- .compras_get(caminho, query)
    if (is.null(resposta)) break
    conteudo <- .compras_json(resposta)
    todas[[length(todas) + 1L]] <- .compras_rbind(conteudo$resultado)
  }
  dt <- data.table::rbindlist(todas, fill = TRUE, use.names = TRUE)
  data.table::setDT(dt)[]
}

#' Variante para endpoints CSV: devolve data.table lido com fread
#' @details Os endpoints *_CSV da API devolvem texto separado por ";",
#'   convencao brasileira (decimal ","), com nomes de colunas em snake_case
#'   (diferente do camelCase do JSON) e uma ultima linha de rodape com o
#'   envelope de paginacao (\code{totalRegistros:N;totalPaginas:N;
#'   paginasRestantes:N}). O rodape e removido do resultado e seus valores
#'   controlam a paginacao automatica.
#' @noRd
.compras_get_csv <- function(caminho, query = NULL, tamanho_pagina = NULL,
                             paginar = TRUE, quieto = FALSE,
                             resposta_stub = NULL) {
  query <- query %||% list()
  if (!is.null(tamanho_pagina)) {
    if (tamanho_pagina < 10 && tamanho_pagina >= 0) {
      stop("tamanho_pagina deve ser no minimo 10 (validacao da API).", call. = FALSE)
    }
    query$tamanhoPagina <- as.integer(tamanho_pagina)
  }
  if (is.null(query$pagina)) query$pagina <- 1L

  ler_csv <- function(resposta) {
    txt <- httr::content(resposta, "text", encoding = "UTF-8", as = "text")
    linhas <- strsplit(txt, "\r?\n", perl = TRUE)[[1L]]
    if (!length(linhas) || all(!nzchar(trimws(linhas)))) {
      return(list(dados = data.table::data.table(),
                  info = list(totalRegistros = NA_integer_,
                              totalPaginas = NA_integer_,
                              paginasRestantes = NA_integer_)))
    }
    eh_rodape <- grepl("^\\s*totalRegistros:", linhas)
    rodape <- linhas[eh_rodape]
    linhas <- linhas[!eh_rodape]
    info <- list(totalRegistros = NA_integer_, totalPaginas = NA_integer_,
                 paginasRestantes = NA_integer_)
    if (length(rodape) == 1L) {
      extrair <- function(campo) {
        m <- regmatches(rodape, regexpr(paste0(campo, ":\\s*[0-9]+"), rodape))
        if (length(m) == 1L && nchar(m) > 0L) {
          as.integer(sub(paste0(campo, ":\\s*"), "", m))
        } else {
          NA_integer_
        }
      }
      info$totalRegistros <- extrair("totalRegistros")
      info$totalPaginas <- extrair("totalPaginas")
      info$paginasRestantes <- extrair("paginasRestantes")
    }
    dt <- data.table::fread(
      text = paste(linhas, collapse = "\n"), sep = ";", dec = ",",
      encoding = "UTF-8", showProgress = FALSE, fill = TRUE
    )
    list(dados = data.table::setDT(dt)[], info = info)
  }

  resposta <- resposta_stub %||% .compras_get(caminho, query)
  if (is.null(resposta)) return(invisible(NULL))
  lido <- ler_csv(resposta)
  registros <- lido$dados
  info <- lido$info

  if (!paginar || !is.null(resposta_stub)) return(registros)

  total_paginas <- info$totalPaginas
  if (isTRUE(total_paginas > 1L) && !quieto) {
    message("Consulta com ", total_paginas, " paginas (",
            info$totalRegistros, " registros). Buscando...")
  }
  todas <- list(registros)
  while (isTRUE(info$paginasRestantes > 0L)) {
    query$pagina <- query$pagina + 1L
    resposta <- .compras_get(caminho, query)
    if (is.null(resposta)) break
    lido <- ler_csv(resposta)
    if (!nrow(lido$dados)) break
    todas[[length(todas) + 1L]] <- lido$dados
    info <- lido$info
  }
  dt <- data.table::rbindlist(todas, fill = TRUE, use.names = TRUE)
  data.table::setDT(dt)[]
}

#' Operador de coalescencia nula (compatibilidade com R >= 4.1)
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
