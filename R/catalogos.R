# Catálogos hierárquicos de materiais e serviços do Compras.gov.br.
#
# Hierarquia do catálogo de materiais: Grupo > Classe > PDM > Item.
# Hierarquia do catálogo de serviços: Seção > Divisão > Grupo > Classe >
#   Subclasse > Item.
# Todos os wrappers recebem os filtros do respectivo endpoint em snake_case
# e os argumentos comuns de paginação.

## Catálogo de materiais ------------------------------------------------------

#' Consulta grupos do catálogo de materiais
#'
#' Primeiro nível da hierarquia de materiais (Grupo > Classe > PDM > Item).
#'
#' @param codigo_grupo Código do grupo.
#' @param status_grupo Filtra por status (TRUE = ativo).
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas.
#' @param tamanho_pagina Tamanho da página (mínimo 10; padrão da API: 500).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com os grupos de materiais.
#' @export
#' @examples
#' \dontrun{grupos <- material_grupo()}
material_grupo <- function(codigo_grupo = NULL, status_grupo = NULL,
                           paginar = TRUE, tamanho_pagina = NULL,
                           quieto = FALSE) {
  compras_consultar(
    "modulo-material/1_consultarGrupoMaterial",
    codigoGrupo = codigo_grupo, statusGrupo = status_grupo,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta classes do catálogo de materiais
#'
#' Segundo nível da hierarquia (Grupo > Classe > PDM > Item).
#'
#' @param codigo_grupo Filtra por grupo.
#' @param codigo_classe Código da classe.
#' @param status_classe Filtra por status (TRUE = ativo).
#' @param bps Filtra por Baleiro/BPS.
#' @inheritParams material_grupo
#' @return Um \code{data.table} com as classes de materiais.
#' @export
#' @examples
#' \dontrun{classes <- material_classe(codigo_grupo = 10)}
material_classe <- function(codigo_grupo = NULL, codigo_classe = NULL,
                            status_classe = NULL, bps = NULL,
                            paginar = TRUE, tamanho_pagina = NULL,
                            quieto = FALSE) {
  compras_consultar(
    "modulo-material/2_consultarClasseMaterial",
    codigoGrupo = codigo_grupo, codigoClasse = codigo_classe,
    statusClasse = status_classe, bps = bps,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta PDMs do catálogo de materiais
#'
#' Terceiro nível da hierarquia (Grupo > Classe > PDM > Item). PDM é o
#' "Padrão Descritivo de Material".
#'
#' @param status_pdm Filtra por status (TRUE = ativo).
#' @param codigo_pdm Código do PDM.
#' @inheritParams material_classe
#' @return Um \code{data.table} com os PDMs.
#' @export
#' @examples
#' \dontrun{pdms <- material_pdm(codigo_grupo = 10)}
material_pdm <- function(codigo_pdm = NULL, codigo_grupo = NULL,
                         codigo_classe = NULL, status_pdm = NULL, bps = NULL,
                         paginar = TRUE, tamanho_pagina = NULL,
                         quieto = FALSE) {
  compras_consultar(
    "modulo-material/3_consultarPdmMaterial",
    codigoPdm = codigo_pdm, codigoGrupo = codigo_grupo,
    codigoClasse = codigo_classe, statusPdm = status_pdm, bps = bps,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta itens do catálogo de materiais
#'
#' Último nível da hierarquia (Grupo > Classe > PDM > Item). Cada item já
#' traz as descrições de todos os níveis superiores, permitindo montar a
#' tabela hierárquica completa a partir deste único endpoint.
#'
#' @param codigo_item Código do item de catálogo.
#' @param descricao_item Descrição (aceita parte do texto).
#' @param status_item Filtra por status (TRUE = ativo).
#' @param codigo_ncm Código NCM (Nomenclatura Comum do Mercosul).
#' @inheritParams material_classe
#' @inheritParams material_pdm
#' @return Um \code{data.table} com os itens de catálogo de materiais.
#' @export
#' @examples
#' \dontrun{itens <- material_item(codigo_grupo = 10, tamanho_pagina = 100)}
material_item <- function(codigo_item = NULL, codigo_grupo = NULL,
                          codigo_classe = NULL, codigo_pdm = NULL,
                          descricao_item = NULL, status_item = NULL,
                          bps = NULL, codigo_ncm = NULL,
                          paginar = TRUE, tamanho_pagina = NULL,
                          quieto = FALSE) {
  compras_consultar(
    "modulo-material/4_consultarItemMaterial",
    codigoItem = codigo_item, codigoGrupo = codigo_grupo,
    codigoClasse = codigo_classe, codigoPdm = codigo_pdm,
    descricaoItem = descricao_item, statusItem = status_item,
    bps = bps, codigo_ncm = codigo_ncm,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta naturezas de despesa dos PDMs de materiais
#' @param codigo_natureza_despesa Código da natureza de despesa.
#' @param status_natureza_despesa Filtra por status.
#' @inheritParams material_pdm
#' @return Um \code{data.table} com a relação PDM x natureza de despesa.
#' @export
#' @examples
#' \dontrun{nd <- material_natureza_despesa(codigo_pdm = 2624)}
material_natureza_despesa <- function(codigo_pdm = NULL,
                                      codigo_natureza_despesa = NULL,
                                      status_natureza_despesa = NULL,
                                      paginar = TRUE, tamanho_pagina = NULL,
                                      quieto = FALSE) {
  compras_consultar(
    "modulo-material/5_consultarMaterialNaturezaDespesa",
    codigoPdm = codigo_pdm, codigoNaturezaDespesa = codigo_natureza_despesa,
    statusNaturezaDespesa = status_natureza_despesa,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta unidades de fornecimento dos PDMs de materiais
#' @param status_unidade_fornecimento Filtra por status.
#' @inheritParams material_pdm
#' @return Um \code{data.table} com a relação PDM x unidade de fornecimento.
#' @export
#' @examples
#' \dontrun{uf <- material_unidade_fornecimento(codigo_pdm = 2624)}
material_unidade_fornecimento <- function(codigo_pdm = NULL,
                                          status_unidade_fornecimento = NULL,
                                          paginar = TRUE,
                                          tamanho_pagina = NULL,
                                          quieto = FALSE) {
  compras_consultar(
    "modulo-material/6_consultarMaterialUnidadeFornecimento",
    codigoPdm = codigo_pdm,
    statusUnidadeFornecimentoPdm = status_unidade_fornecimento,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta características de itens de materiais
#' @param codigo_item Código do item de catálogo.
#' @inheritParams material_grupo
#' @return Um \code{data.table} com as características do item.
#' @export
#' @examples
#' \dontrun{car <- material_caracteristicas(codigo_item = 445025)}
material_caracteristicas <- function(codigo_item, paginar = TRUE,
                                     tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-material/7_consultarMaterialCaracteristicas",
    codigoItem = codigo_item,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

## Catálogo de serviços -------------------------------------------------------

#' Consulta seções do catálogo de serviços
#'
#' Primeiro nível da hierarquia de serviços (Seção > Divisão > Grupo >
#' Classe > Subclasse > Item).
#'
#' @param codigo_secao Código da seção.
#' @param status_secao Filtra por status (TRUE = ativo).
#' @inheritParams material_grupo
#' @return Um \code{data.table} com as seções de serviços.
#' @export
#' @examples
#' \dontrun{secoes <- servico_secao()}
servico_secao <- function(codigo_secao = NULL, status_secao = NULL,
                          paginar = TRUE, tamanho_pagina = NULL,
                          quieto = FALSE) {
  compras_consultar(
    "modulo-servico/1_consultarSecaoServico",
    codigoSecao = codigo_secao, statusSecao = status_secao,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta divisões do catálogo de serviços
#' @param codigo_divisao Código da divisão.
#' @param status_divisao Filtra por status.
#' @inheritParams servico_secao
#' @return Um \code{data.table} com as divisões de serviços.
#' @export
#' @examples
#' \dontrun{div <- servico_divisao(codigo_secao = 1)}
servico_divisao <- function(codigo_secao = NULL, codigo_divisao = NULL,
                            status_divisao = NULL, paginar = TRUE,
                            tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-servico/2_consultarDivisaoServico",
    codigoSecao = codigo_secao, codigoDivisao = codigo_divisao,
    statusDivisao = status_divisao,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta grupos do catálogo de serviços
#' @param codigo_grupo Código do grupo de serviço.
#' @param status_grupo Filtra por status.
#' @inheritParams servico_divisao
#' @return Um \code{data.table} com os grupos de serviços.
#' @export
#' @examples
#' \dontrun{grp <- servico_grupo(codigo_divisao = 3)}
servico_grupo <- function(codigo_divisao = NULL, codigo_grupo = NULL,
                          status_grupo = NULL, paginar = TRUE,
                          tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-servico/3_consultarGrupoServico",
    codigoDivisao = codigo_divisao, codigoGrupo = codigo_grupo,
    statusGrupo = status_grupo,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta classes do catálogo de serviços
#' @inheritParams servico_grupo
#' @param codigo_classe Código da classe.
#' @return Um \code{data.table} com as classes de serviços.
#' @export
#' @examples
#' \dontrun{cls <- servico_classe(codigo_grupo = 2)}
servico_classe <- function(codigo_grupo = NULL, codigo_classe = NULL,
                           status_grupo = NULL, paginar = TRUE,
                           tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-servico/4_consultarClasseServico",
    codigoGrupo = codigo_grupo, codigoClasse = codigo_classe,
    statusGrupo = status_grupo,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta subclasses do catálogo de serviços
#' @param codigo_subclasse Código da subclasse.
#' @param status_subclasse Filtra por status.
#' @inheritParams servico_classe
#' @return Um \code{data.table} com as subclasses de serviços.
#' @export
#' @examples
#' \dontrun{sub <- servico_subclasse(codigo_classe = 1)}
servico_subclasse <- function(codigo_classe = NULL, codigo_subclasse = NULL,
                              status_subclasse = NULL, paginar = TRUE,
                              tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-servico/5_consultarSubClasseServico",
    codigoClasse = codigo_classe, codigoSubclasse = codigo_subclasse,
    statusSubclasse = status_subclasse,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta itens do catálogo de serviços
#'
#' Último nível da hierarquia de serviços. Cada item já traz as descrições
#' de todos os níveis superiores.
#'
#' @param codigo_cpc Código Comum de Procurement (CPC).
#' @param codigo_servico Código do serviço.
#' @param exclusivo_central_compras Indica exclusividade de central de compras.
#' @param status_servico Filtra por status.
#' @inheritParams servico_secao
#' @inheritParams servico_divisao
#' @inheritParams servico_classe
#' @inheritParams servico_subclasse
#' @return Um \code{data.table} com os itens de catálogo de serviços.
#' @export
#' @examples
#' \dontrun{itens <- servico_item(codigo_secao = 1, tamanho_pagina = 100)}
servico_item <- function(codigo_secao = NULL, codigo_divisao = NULL,
                         codigo_grupo = NULL, codigo_classe = NULL,
                         codigo_subclasse = NULL, codigo_cpc = NULL,
                         codigo_servico = NULL,
                         exclusivo_central_compras = NULL,
                         status_servico = NULL, paginar = TRUE,
                         tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-servico/6_consultarItemServico",
    codigoSecao = codigo_secao, codigoDivisao = codigo_divisao,
    codigoGrupo = codigo_grupo, codigoClasse = codigo_classe,
    codigoSubclasse = codigo_subclasse, codigoCpc = codigo_cpc,
    codigoServico = codigo_servico,
    exclusivoCentralCompras = exclusivo_central_compras,
    statusServico = status_servico,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta unidades de medida de serviços
#' @param status_unidade_medida Filtra por status.
#' @inheritParams servico_secao
#' @param codigo_servico Código do serviço.
#' @return Um \code{data.table} com as unidades de medida.
#' @export
#' @examples
#' \dontrun{um <- servico_unidade_medida(codigo_servico = 10490)}
servico_unidade_medida <- function(codigo_servico = NULL,
                                   status_unidade_medida = NULL,
                                   paginar = TRUE, tamanho_pagina = NULL,
                                   quieto = FALSE) {
  compras_consultar(
    "modulo-servico/7_consultarUndMedidaServico",
    codigoServico = codigo_servico,
    statusUnidadeMedida = status_unidade_medida,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta naturezas de despesa de serviços
#' @inheritParams material_natureza_despesa
#' @inheritParams servico_unidade_medida
#' @return Um \code{data.table} com a relação serviço x natureza de despesa.
#' @export
#' @examples
#' \dontrun{nd <- servico_natureza_despesa(codigo_servico = 10490)}
servico_natureza_despesa <- function(codigo_servico = NULL,
                                     codigo_natureza_despesa = NULL,
                                     status_natureza_despesa = NULL,
                                     paginar = TRUE, tamanho_pagina = NULL,
                                     quieto = FALSE) {
  compras_consultar(
    "modulo-servico/8_consultarNaturezaDespesaServico",
    codigoServico = codigo_servico,
    codigoNaturezaDespesa = codigo_natureza_despesa,
    statusNaturezaDespesa = status_natureza_despesa,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}
