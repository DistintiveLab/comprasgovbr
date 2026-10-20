# Gerador da tabela de metadados hierárquica da API de Dados Abertos
# do Compras.gov.br a partir do documento OpenAPI oficial.
#
# Fonte: https://dadosabertos.compras.gov.br/v3/api-docs
# Cópia do documento usada: data-raw/compras_api_spec.json
#
# Saída: data/metadados.rda (data.table "metadados")

library(jsonlite)
library(data.table)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

spec_path <- file.path("data-raw", "compras_api_spec.json")
spec <- jsonlite::fromJSON(spec_path, simplifyVector = FALSE)

base_url <- spec$servers[[1]]$url

# A especificacao oficial nao traz o campo `summary` das operacoes; o rotulo
# humanamente legivel e derivado do operationId (remove o prefixo numerico e
# separa palavras camelCase).
derivar_sumario <- function(operation_id) {
  rotulo <- sub("^[0-9]+(\\.[0-9]+)?_", "", operation_id)
  rotulo <- gsub("(\\p{Ll})(\\p{Lu})", "\\1 \\2", rotulo, perl = TRUE)
  paste0(toupper(substring(rotulo, 1, 1)), substring(rotulo, 2))
}

linhas <- list()
i <- 0L

for (caminho in names(spec$paths)) {
  item <- spec$paths[[caminho]]
  for (verbo in names(item)) {
    if (!verbo %in% c("get", "post", "put", "delete", "patch")) next
    op <- item[[verbo]]
    tag <- if (length(op$tags)) op$tags[[1]] else NA_character_
    partes <- strsplit(caminho, "/", fixed = TRUE)[[1]]
    modulo <- partes[partes != ""][1]

    params <- op$parameters
    if (!length(params)) params <- list()

    if (!length(params)) {
      i <- i + 1L
      linhas[[i]] <- data.table::data.table(
        tag = tag, modulo = modulo, caminho = caminho, operacao = op$operationId,
        verbo = toupper(verbo), sumario = derivar_sumario(op$operationId),
        parametro = NA_character_, em = NA_character_, obrigatorio = NA,
        tipo = NA_character_, formato = NA_character_, padrao = NA_character_,
        minimo = NA_character_, valores_aceitos = NA_character_,
        descricao = NA_character_
      )
      next
    }

    for (p in params) {
      sch <- p$schema
      if (is.null(sch)) sch <- list()
      i <- i + 1L
      linhas[[i]] <- data.table::data.table(
        tag = tag, modulo = modulo, caminho = caminho, operacao = op$operationId,
        verbo = toupper(verbo), sumario = derivar_sumario(op$operationId),
        parametro = p$name,
        em = p$`in` %||% NA_character_,
        obrigatorio = isTRUE(p$required),
        tipo = sch$type %||% NA_character_,
        formato = sch$format %||% NA_character_,
        padrao = as.character(sch$default)[1] %||% NA_character_,
        minimo = as.character(sch$minimum)[1] %||% NA_character_,
        valores_aceitos = if (length(sch$enum)) paste(unlist(sch$enum), collapse = ",") else NA_character_,
        descricao = p$description %||% NA_character_
      )
    }
  }
}

metadados <- data.table::rbindlist(linhas, fill = TRUE)

# Ordenação hierárquica: tag (módulo funcional numerado) > caminho > ordem dos parâmetros
metadados[, ordem_parametro := seq_len(.N), by = .(caminho, verbo)]
data.table::setorder(metadados, tag, caminho, verbo, ordem_parametro)

# Atributos de contexto úteis para consulta e depuração
attr(metadados, "base_url") <- base_url
attr(metadados, "versao_api") <- spec$info$version
attr(metadados, "especificacao") <- "https://dadosabertos.compras.gov.br/v3/api-docs"
attr(metadados, "data_geracao") <- format(Sys.Date())

save(metadados, file = file.path("data", "metadados.rda"))

cat("Registros:", nrow(metadados), "\n")
cat("Endpoints distintos:", uniqueN(metadados[, .(caminho, verbo)]), "\n")
cat("Módulos:", uniqueN(metadados$modulo), "\n")
print(metadados[, .N, by = tag][order(tag)])
