#' Tabela de metadados hierárquica da API de Dados Abertos do Compras.gov.br
#'
#' Data set interno com o mapeamento completo dos módulos, endpoints e
#' parâmetros da API, extraído do documento OpenAPI oficial em
#' \url{https://dadosabertos.compras.gov.br/v3/api-docs}. Cada linha
#' corresponde a um parâmetro de um endpoint; endpoints sem parâmetros
#' aparecem uma única vez com \code{parametro} ausente.
#'
#' @docType data
#' @usage data(metadados)
#' @format Um \code{data.table} com 478 linhas e as colunas:
#' \itemize{
#'   \item{\code{tag}: módulo funcional numerado (ex.: "01 - CATÁLOGO - MATERIAL").}
#'   \item{\code{modulo}: prefixo do caminho (ex.: "modulo-material").}
#'   \item{\code{caminho}: caminho do endpoint na URL.}
#'   \item{\code{operacao}: \code{operationId} do endpoint.}
#'   \item{\code{verbo}: verbo HTTP.}
#'   \item{\code{sumario}: resumo legível da operação, derivado do
#'     \code{operationId} (a especificação oficial não traz esse campo).}
#'   \item{\code{parametro}: nome do parâmetro de consulta.}
#'   \item{\code{em}: localização do parâmetro (query, path).}
#'   \item{\code{obrigatorio}: lógico; parâmetro obrigatório?}
#'   \item{\code{tipo}, \code{formato}: tipo e formato do parâmetro.}
#'   \item{\code{padrao}, \code{minimo}: valor padrão e mínimo aceitos.}
#'   \item{\code{valores_aceitos}: valores de enumeração, separados por vírgula.}
#'   \item{\code{descricao}: descrição do parâmetro, quando disponível.}
#' }
#' @keywords metadados datasets
#' @source \url{https://dadosabertos.compras.gov.br/v3/api-docs}
#' @examples
#' data(metadados)
#' head(metadados)
"metadados"

#' Consulta os metadados hierárquicos da API do Compras.gov.br
#'
#' Devolve a tabela de metadados completa da API de Dados Abertos do
#' Compras.gov.br, permitindo filtrar por módulo, endpoint, operação ou
#' parâmetro. Use esta função para descobrir quais endpoints existem, quais
#' parâmetros exigem e quais valores aceitam, antes de chamar
#' \code{\link{compras_consultar}} ou as funções de conveniência do pacote.
#'
#' @param modulo Vetor opcional com prefixos de módulo a filtrar
#'   (ex.: \code{"modulo-material"}). Aceita também as tags funcionais
#'   (ex.: \code{"01 - CATÁLOGO - MATERIAL"}).
#' @param caminho Vetor opcional com caminhos de endpoint a filtrar.
#' @param operacao Vetor opcional com \code{operationId}s a filtrar.
#' @param ... Filtros adicionais passados como pares nome-valor sobre
#'   quaisquer colunas da tabela (ex.: \code{obrigatorio = TRUE}). Nos
#'   campos de texto livre \code{sumario} e \code{descricao} o filtro é
#'   parcial (busca pedaço do texto, ignorando maiúsculas).
#'
#' @return Um \code{data.table} filtrado com os metadados.
#' @export
#' @examples
#' # Todos os endpoints do catálogo de materiais
#' compras_metadados(modulo = "modulo-material")
#'
#' # Parâmetros obrigatórios das contratações da Lei 14.133
#' compras_metadados(modulo = "modulo-contratacoes", obrigatorio = TRUE)
compras_metadados <- function(modulo = NULL, caminho = NULL, operacao = NULL, ...) {
  dt <- data.table::as.data.table(comprasgovbr::metadados)
  filtros <- list(...)
  if (!is.null(modulo)) {
    valores_modulo <- modulo
    manter <- dt$modulo %in% valores_modulo | dt$tag %in% valores_modulo
    dt <- dt[manter]
  }
  if (!is.null(caminho)) {
    valores_caminho <- paste0("/", gsub("^/+", "", caminho))
    dt <- dt[dt$caminho %in% valores_caminho]
  }
  if (!is.null(operacao)) {
    valores_operacao <- operacao
    dt <- dt[dt$operacao %in% valores_operacao]
  }
  if (length(filtros)) {
    for (nm in names(filtros)) {
      if (!nm %in% names(dt)) {
        warning("Coluna desconhecida nos filtros: ", nm, call. = FALSE)
        next
      }
      if (nm %in% c("sumario", "descricao")) {
        manter <- grepl(paste(filtros[[nm]], collapse = "|"),
                        dt[[nm]], ignore.case = TRUE)
        dt <- dt[manter]
      } else {
        dt <- dt[match(dt[[nm]], filtros[[nm]], nomatch = 0L) > 0L]
      }
    }
  }
  dt[]
}
