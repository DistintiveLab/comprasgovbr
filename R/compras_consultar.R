#' Consulta genérica à API de Dados Abertos do Compras.gov.br
#'
#' Executa uma consulta a qualquer endpoint \code{GET} da API usando os
#' metadados do pacote para validar o caminho e os parâmetros obrigatórios.
#' Percorre todas as páginas automaticamente e devolve um \code{data.table}.
#' Esta é a função por trás de todas as funções de conveniência do pacote
#' e permite acessar endpoints ainda não cobertos por wrappers dedicados.
#'
#' @param caminho Caminho do endpoint, com ou sem a barra inicial
#'   (ex.: \code{"/modulo-material/1_consultarGrupoMaterial"} ou
#'   \code{"modulo-uasg/1_consultarUasg"}).
#' @param ... Parâmetros de consulta do endpoint, como pares nome-valor
#'   (ex.: \code{codigoGrupo = 10}). \code{NULL} e \code{NA} são ignorados.
#'   Valores lógicos são convertidos para \code{true}/\code{false} e datas
#'   (\code{Date}) para \code{YYYY-MM-DD}, conforme exigido pela API.
#' @param csv Se \code{TRUE} e o endpoint possuir variante \code{_CSV},
#'   consulta a variante CSV (delimitador \code{";"}, decimal \code{","} e
#'   colunas em snake_case, diferente do camelCase do JSON).
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas do
#'   resultado; se \code{FALSE}, retorna apenas a primeira página.
#' @param tamanho_pagina Tamanho da página (a API exige mínimo de 10;
#'   o padrão da API é 500 registros).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#'
#' @return Um \code{data.table} com os registros devolvidos pela API, ou
#'   \code{NULL} invisível (com aviso) em caso de falha de conexão.
#' @export
#' @examples
#' \dontrun{
#' # Grupos do catálogo de materiais
#' compras_consultar("modulo-material/1_consultarGrupoMaterial")
#'
#' # Órgãos do módulo UASG (primeira página apenas)
#' compras_consultar("modulo-uasg/2_consultarOrgao", statusOrgao = TRUE,
#'                   paginar = FALSE)
#' }
compras_consultar <- function(caminho, ..., csv = FALSE, paginar = TRUE,
                              tamanho_pagina = NULL, quieto = FALSE) {
  caminho <- gsub("^/", "", caminho[1])
  caminho_completo <- paste0("/", caminho)

  meta <- comprasgovbr::metadados

  # Resolve a variante CSV quando pedida: a numeração do caminho pode mudar
  # (ex.: 1_consultarUasg -> 1.1_consultarUasg_CSV), portanto o casamento é
  # feito pelo operationId normalizado dentro do mesmo módulo.
  if (isTRUE(csv)) {
    op_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    eh_get <- meta$verbo == "GET"
    eh_base <- meta$caminho == caminho_completo & eh_get
    base_op <- meta$operacao[eh_base][1]
    base_modulo <- meta$modulo[eh_base][1]
    alvo <- paste0(op_norm(base_op), "csv")
    eh_candidato <- eh_get & meta$modulo == base_modulo &
      vapply(meta$operacao, function(op) identical(op_norm(op), alvo),
             logical(1))
    cand <- meta$caminho[eh_candidato]
    if (length(cand)) {
      caminho_completo <- cand[1]
    } else {
      warning(
        "Endpoint '", caminho, "' nao possui variante CSV; usando JSON.",
        call. = FALSE
      )
    }
  }

  if (!caminho_completo %in% meta$caminho) {
    stop(
      "Caminho desconhecido: '", caminho, "'. ",
      "Use compras_metadados() para listar os endpoints disponiveis.",
      call. = FALSE
    )
  }

  eh_endpoint <- meta$caminho == caminho_completo & meta$verbo == "GET"
  meta_ep <- meta[eh_endpoint]

  # Validação de parâmetros obrigatórios
  query <- .compras_query(...)
  obrigatorio_pendente <- !is.na(meta_ep$obrigatorio) & meta_ep$obrigatorio &
    !meta_ep$parametro %in% names(query)
  faltando <- unique(meta_ep$parametro[obrigatorio_pendente])
  if (length(faltando)) {
    stop(
      "Parametro(s) obrigatorios ausentes: ",
      paste(faltando, collapse = ", "), call. = FALSE
    )
  }

  if (isTRUE(csv) && grepl("_CSV$", caminho_completo)) {
    return(.compras_get_csv(
      caminho_completo, query = query, tamanho_pagina = tamanho_pagina,
      paginar = paginar, quieto = quieto
    ))
  }
  .compras_get_paginas(
    caminho_completo, query = query, paginar = paginar,
    tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}
