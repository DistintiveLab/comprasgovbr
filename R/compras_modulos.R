# Wrappers de conveniência para os módulos de dados da API de Dados Abertos
# do Compras.gov.br: pesquisa de preços, UASG/órgãos, PGC, licitações
# (legado), contratações (Lei 14.133), contratos, ARP, fornecedores,
# indicadores e OCDS.

## Pesquisa de preços praticados ------------------------------------------------

#' Pesquisa de preços praticados com materiais
#'
#' Consulta o módulo de Pesquisa de Preços - Preços Praticados para
#' materiais. Os preços são agregados por item de catálogo ou por PDM;
#' use \code{tipo} para escolher a unidade de consulta.
#'
#' @param tipo Tipo do código informado: \code{"codigoItemCatalogo"} ou
#'   \code{"codigoPdm"}.
#' @param codigo Código do item de catálogo ou do PDM, conforme \code{tipo}.
#' @param codigo_uasg Filtra por UASG.
#' @param estado Sigla da UF.
#' @param codigo_municipio Código do município.
#' @param data_resultado Se \code{TRUE}, retorna a data do resultado.
#' @param codigo_classe Código da classe de material.
#' @param poder Esfera do poder: \code{"Executivo"}, \code{"Legislativo"},
#'   \code{"Judiciário"}.
#' @param esfera \code{"Federal"}, \code{"Estadual"} ou \code{"Municipal"}.
#' @param id_compra Identificador da compra.
#' @param data_compra_inicio,data_compra_fim Intervalo da compra
#'   (\code{Date} ou texto \code{"YYYY-MM-DD"}).
#' @param csv Se \code{TRUE}, usa a variante CSV do endpoint (delimitador
#'   \code{";"}, decimal \code{","} e colunas em snake_case, diferente do
#'   camelCase do JSON), que devolve os detalhes por compra.
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas.
#' @param tamanho_pagina Tamanho da página (mínimo 10; padrão da API: 500).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com os preços praticados.
#' @export
#' @examples
#' \dontrun{
#' # Preços praticados de mobiliário de escritório (compras de 2024)
#' movel <- precos_material(tipo = "codigoPdm", codigo = "313",
#'                          data_compra_inicio = "2024-01-01",
#'                          data_compra_fim = "2024-12-31")
#' }
precos_material <- function(tipo, codigo, codigo_uasg = NULL, estado = NULL,
                            codigo_municipio = NULL, data_resultado = NULL,
                            codigo_classe = NULL, poder = NULL, esfera = NULL,
                            id_compra = NULL, data_compra_inicio = NULL,
                            data_compra_fim = NULL, csv = FALSE,
                            paginar = TRUE, tamanho_pagina = NULL,
                            quieto = FALSE) {
  compras_consultar(
    "modulo-pesquisa-preco/1_consultarMaterial",
    tipo = tipo, codigo = codigo, codigoUasg = codigo_uasg,
    estado = estado, codigoMunicipio = codigo_municipio,
    dataResultado = data_resultado, codigoClasse = codigo_classe,
    poder = poder, esfera = esfera, idCompra = id_compra,
    dataCompraInicio = data_compra_inicio,
    dataCompraFim = data_compra_fim,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

#' Detalhe das compras usadas na pesquisa de preços de materiais
#' @param codigo_item_catalogo Código do item de catálogo.
#' @inheritParams precos_material
#' @return Um \code{data.table} com as compras que originaram os preços.
#' @export
#' @examples
#' \dontrun{det <- precos_material_detalhe(codigo_item_catalogo = 445025)}
precos_material_detalhe <- function(codigo_item_catalogo,
                                    data_compra_inicio = NULL,
                                    data_compra_fim = NULL, csv = FALSE,
                                    paginar = TRUE, tamanho_pagina = NULL,
                                    quieto = FALSE) {
  compras_consultar(
    "modulo-pesquisa-preco/2_consultarMaterialDetalhe",
    codigoItemCatalogo = codigo_item_catalogo,
    dataCompraInicio = data_compra_inicio,
    dataCompraFim = data_compra_fim,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

#' Pesquisa de preços praticados com serviços
#' @param codigo_item_catalogo Código do item de catálogo de serviço.
#' @inheritParams precos_material
#' @return Um \code{data.table} com os preços praticados do serviço.
#' @export
#' @examples
#' \dontrun{pr <- precos_servico(codigo_item_catalogo = 16770)}
precos_servico <- function(codigo_item_catalogo, codigo_uasg = NULL,
                           estado = NULL, codigo_municipio = NULL,
                           data_resultado = NULL, poder = NULL,
                           esfera = NULL, data_compra_inicio = NULL,
                           data_compra_fim = NULL, id_compra = NULL,
                           csv = FALSE, paginar = TRUE,
                           tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-pesquisa-preco/3_consultarServico",
    codigoItemCatalogo = codigo_item_catalogo, codigoUasg = codigo_uasg,
    estado = estado, codigoMunicipio = codigo_municipio,
    dataResultado = data_resultado, poder = poder, esfera = esfera,
    dataCompraInicio = data_compra_inicio,
    dataCompraFim = data_compra_fim, idCompra = id_compra,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

#' Detalhe das compras usadas na pesquisa de preços de serviços
#' @inheritParams precos_material_detalhe
#' @return Um \code{data.table} com as compras que originaram os preços.
#' @export
#' @examples
#' \dontrun{det <- precos_servico_detalhe(codigo_item_catalogo = 16770)}
precos_servico_detalhe <- function(codigo_item_catalogo,
                                   data_compra_inicio = NULL,
                                   data_compra_fim = NULL, csv = FALSE,
                                   paginar = TRUE, tamanho_pagina = NULL,
                                   quieto = FALSE) {
  compras_consultar(
    "modulo-pesquisa-preco/4_consultarServicoDetalhe",
    codigoItemCatalogo = codigo_item_catalogo,
    dataCompraInicio = data_compra_inicio,
    dataCompraFim = data_compra_fim,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

## UASG e órgãos ---------------------------------------------------------------

#' Consulta UASGs (Unidades Administrativas de Serviços Gerais)
#'
#' @param codigo_uasg Código da UASG.
#' @param uso_sisg Indica se usa o SIAFI/SISG.
#' @param cnpj_cpf_orgao CNPJ/CPF do órgão.
#' @param cnpj_cpf_orgao_vinculado CNPJ/CPF do órgão vinculado.
#' @param cnpj_cpf_orgao_superior CNPJ/CPF do órgão superior.
#' @param sigla_uf Sigla da UF.
#' @param status_uasg Filtra por status (obrigatório na API; padrão
#'   \code{TRUE}, UASGs ativas).
#' @param csv Se \code{TRUE}, usa a variante CSV do endpoint.
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas.
#' @param tamanho_pagina Tamanho da página (mínimo 10; padrão da API: 500).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com as UASGs.
#' @export
#' @examples
#' \dontrun{u <- uasgs(sigla_uf = "DF", tamanho_pagina = 100)}
uasgs <- function(codigo_uasg = NULL, uso_sisg = NULL,
                  cnpj_cpf_orgao = NULL, cnpj_cpf_orgao_vinculado = NULL,
                  cnpj_cpf_orgao_superior = NULL, sigla_uf = NULL,
                  status_uasg = TRUE, csv = FALSE, paginar = TRUE,
                  tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-uasg/1_consultarUasg",
    codigoUasg = codigo_uasg, usoSisg = uso_sisg,
    cnpjCpfOrgao = cnpj_cpf_orgao,
    cnpjCpfOrgaoVinculado = cnpj_cpf_orgao_vinculado,
    cnpjCpfOrgaoSuperior = cnpj_cpf_orgao_superior,
    siglaUf = sigla_uf, statusUasg = status_uasg,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

#' Consulta órgãos vinculados às UASGs
#' @param codigo_orgao Código do órgão.
#' @param status_orgao Filtra por status (obrigatório na API; padrão
#'   \code{TRUE}, órgãos ativos).
#' @inheritParams uasgs
#' @return Um \code{data.table} com os órgãos.
#' @export
#' @examples
#' \dontrun{o <- orgaos(status_orgao = TRUE, paginar = FALSE)}
orgaos <- function(cnpj_cpf_orgao = NULL, cnpj_cpf_orgao_vinculado = NULL,
                   cnpj_cpf_orgao_superior = NULL, codigo_orgao = NULL,
                   status_orgao = TRUE, uso_sisg = NULL, csv = FALSE,
                   paginar = TRUE, tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-uasg/2_consultarOrgao",
    cnpjCpfOrgao = cnpj_cpf_orgao,
    cnpjCpfOrgaoVinculado = cnpj_cpf_orgao_vinculado,
    cnpjCpfOrgaoSuperior = cnpj_cpf_orgao_superior,
    codigoOrgao = codigo_orgao, statusOrgao = status_orgao,
    usoSisg = uso_sisg,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

## PGC (Plano de Gerenciamento de Compras) --------------------------------------

#' Detalhe do PGC por órgão
#'
#' @param orgao CNPJ do órgão.
#' @param ano_pca Ano do Plano de Contratações Anual (PCA).
#' @param codigo_uasg Filtra por UASG.
#' @param csv Se \code{TRUE}, usa a variante CSV do endpoint.
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas.
#' @param tamanho_pagina Tamanho da página (mínimo 10; padrão da API: 500).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com os itens do PGC.
#' @export
#' @examples
#' \dontrun{pgc <- pgc_detalhe(orgao = "00394460000100", ano_pca = 2025)}
pgc_detalhe <- function(orgao, ano_pca, codigo_uasg = NULL, csv = FALSE,
                        paginar = TRUE, tamanho_pagina = NULL,
                        quieto = FALSE) {
  compras_consultar(
    "modulo-pgc/1_consultarPgcDetalhe",
    orgao = orgao, anoPcaProjetoCompra = ano_pca, codigoUasg = codigo_uasg,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

#' Detalhe do PGC por item de catálogo
#'
#' @param tipo Tipo do código: \code{"Material"} ou \code{"Servico"}.
#' @param codigo Código do item de catálogo.
#' @inheritParams pgc_detalhe
#' @return Um \code{data.table} com o detalhe por item de catálogo.
#' @export
#' @examples
#' \dontrun{pgc2 <- pgc_detalhe_catalogo(ano_pca = 2025, tipo = "Material",
#'                                      codigo = "445025")}
pgc_detalhe_catalogo <- function(ano_pca, tipo, codigo, csv = FALSE,
                                 paginar = TRUE, tamanho_pagina = NULL,
                                 quieto = FALSE) {
  compras_consultar(
    "modulo-pgc/2_consultarPgcDetalheCatalogo",
    anoPcaProjetoCompra = ano_pca, tipo = tipo, codigo = codigo,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

#' Agregação do PGC por órgão e ano
#'
#' @param ano Ano de referência.
#' @inheritParams pgc_detalhe
#' @return Um \code{data.table} com a agregação do PGC.
#' @export
#' @examples
#' \dontrun{agg <- pgc_agregacao(orgao = "00394460000100", ano = 2025)}
pgc_agregacao <- function(orgao, ano, csv = FALSE, paginar = TRUE,
                          tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-pgc/3_consultarPgcAgregacao",
    orgao = orgao, ano = ano,
    csv = csv, paginar = paginar, tamanho_pagina = tamanho_pagina,
    quieto = quieto
  )
}

## Licitações (módulo legado) ----------------------------------------------------

#' Consulta licitações (módulo legado, pré-Lei 14.133)
#'
#' @param uasg Código da UASG.
#' @param numero_aviso Número do aviso da licitação.
#' @param modalidade Código da modalidade.
#' @param data_publicacao_inicial,data_publicacao_final Intervalo de
#'   publicação do aviso (\code{Date} ou \code{"YYYY-MM-DD"}).
#' @param pertence_14133 Indica se a compra pertence à Lei 14.133.
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas.
#' @param tamanho_pagina Tamanho da página (mínimo 10; padrão da API: 500).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com as licitações.
#' @export
#' @examples
#' \dontrun{lic <- licitacoes(data_publicacao_inicial = "2024-01-01",
#'                            data_publicacao_final = "2024-01-31")}
licitacoes <- function(uasg = NULL, numero_aviso = NULL, modalidade = NULL,
                       data_publicacao_inicial, data_publicacao_final,
                       pertence_14133 = NULL, paginar = TRUE,
                       tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-legado/1_consultarLicitacao",
    uasg = uasg, numero_aviso = numero_aviso, modalidade = modalidade,
    data_publicacao_inicial = data_publicacao_inicial,
    data_publicacao_final = data_publicacao_final,
    pertence14133 = pertence_14133,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta uma licitação por identificador
#' @param id_compra Identificador da compra.
#' @param dt_alteracao Filtro por data de alteração.
#' @return Um \code{data.table} com a licitação.
#' @export
#' @examples
#' \dontrun{lic <- licitacao_id(id_compra = "16019405000152023")}
licitacao_id <- function(id_compra, dt_alteracao = NULL) {
  compras_consultar(
    "modulo-legado/1.1_consultarLicitacao_Id",
    id_compra = id_compra, dt_alteracao = dt_alteracao
  )
}

#' Consulta itens de licitações (módulo legado)
#' @inheritParams licitacoes
#' @param decreto_7174 Indica Decreto 7.174.
#' @param codigo_item_material Código do item de material.
#' @param codigo_item_servico Código do item de serviço.
#' @param cnpj_fornecedor CNPJ do fornecedor vencedor.
#' @param cpf_vencedor CPF do fornecedor vencedor.
#' @return Um \code{data.table} com os itens da licitação.
#' @export
#' @examples
#' \dontrun{it <- itens_licitacao(uasg = 160194, numero_aviso = 152023,
#'                                modalidade = 5)}
itens_licitacao <- function(uasg = NULL, numero_aviso = NULL, modalidade,
                            decreto_7174 = NULL, codigo_item_material = NULL,
                            codigo_item_servico = NULL, cnpj_fornecedor = NULL,
                            cpf_vencedor = NULL, paginar = TRUE,
                            tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-legado/2_consultarItemLicitacao",
    uasg = uasg, numero_aviso = numero_aviso, modalidade = modalidade,
    decreto_7174 = decreto_7174,
    codigo_item_material = codigo_item_material,
    codigo_item_servico = codigo_item_servico,
    cnpj_fornecedor = cnpj_fornecedor, cpfVencedor = cpf_vencedor,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta pregões (módulo legado)
#' @param co_uasg Código da UASG.
#' @param co_orgao Código do órgão.
#' @param numero Número do pregão.
#' @param ds_tipo_pregao_compra Tipo do pregão/compra.
#' @param dt_data_edital_inicial,dt_data_edital_final Intervalo da data do
#'   edital (\code{Date} ou \code{"YYYY-MM-DD"}).
#' @inheritParams licitacoes
#' @return Um \code{data.table} com os pregões.
#' @export
#' @examples
#' \dontrun{preg <- pregoes(dt_data_edital_inicial = "2024-01-01",
#'                          dt_data_edital_final = "2024-01-31")}
pregoes <- function(co_uasg = NULL, co_orgao = NULL, numero = NULL,
                    ds_tipo_pregao_compra = NULL, dt_data_edital_inicial,
                    dt_data_edital_final, pertence_14133 = NULL,
                    paginar = TRUE, tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-legado/3_consultarPregoes",
    co_uasg = co_uasg, co_orgao = co_orgao, numero = numero,
    ds_tipo_pregao_compra = ds_tipo_pregao_compra,
    dt_data_edital_inicial = dt_data_edital_inicial,
    dt_data_edital_final = dt_data_edital_final,
    pertence14133 = pertence_14133,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta itens de pregões (módulo legado)
#' @param fornecedor_vencedor CNPJ/CPF do vencedor.
#' @param dt_hom_inicial,dt_hom_final Intervalo de homologação.
#' @inheritParams pregoes
#' @inheritParams itens_licitacao
#' @return Um \code{data.table} com os itens dos pregões.
#' @export
#' @examples
#' \dontrun{itp <- itens_pregoes(dt_hom_inicial = "2024-01-01",
#'                               dt_hom_final = "2024-01-31")}
itens_pregoes <- function(co_uasg = NULL, decreto_7174 = NULL,
                          fornecedor_vencedor = NULL, dt_hom_inicial,
                          dt_hom_final, paginar = TRUE,
                          tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-legado/4_consultarItensPregoes",
    co_uasg = co_uasg, decreto_7174 = decreto_7174,
    fornecedor_vencedor = fornecedor_vencedor,
    dt_hom_inicial = dt_hom_inicial, dt_hom_final = dt_hom_final,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta compras sem licitação / dispensas (módulo legado)
#' @param dt_ano_aviso Ano do aviso (obrigatório).
#' @param nu_aviso_licitacao Número do aviso.
#' @param co_modalidade_licitacao Código da modalidade.
#' @param co_orgao,co_orgao_superior,co_uasg Códigos de órgão e UASG.
#' @param dt_declaracao_dispensa_inicial,dt_declaracao_dispensa_final
#'   Intervalo da declaração de dispensa.
#' @param dt_ratificacao,dt_publicacao Datas de ratificação e publicação.
#' @inheritParams licitacoes
#' @return Um \code{data.table} com as compras sem licitação.
#' @export
#' @examples
#' \dontrun{csl <- compras_sem_licitacao(dt_ano_aviso = 2024)}
compras_sem_licitacao <- function(dt_ano_aviso, nu_aviso_licitacao = NULL,
                                  co_modalidade_licitacao = NULL,
                                  co_orgao = NULL, co_orgao_superior = NULL,
                                  co_uasg = NULL,
                                  dt_declaracao_dispensa_inicial = NULL,
                                  dt_declaracao_dispensa_final = NULL,
                                  dt_ratificacao = NULL, dt_publicacao = NULL,
                                  pertence_14133 = NULL, paginar = TRUE,
                                  tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-legado/5_consultarComprasSemLicitacao",
    dt_ano_aviso = dt_ano_aviso,
    nu_aviso_licitacao = nu_aviso_licitacao,
    co_modalidade_licitacao = co_modalidade_licitacao,
    co_orgao = co_orgao, co_orgao_superior = co_orgao_superior,
    co_uasg = co_uasg,
    dtDeclaracaoDispensaInicial = dt_declaracao_dispensa_inicial,
    dtDeclaracaoDispensaFinal = dt_declaracao_dispensa_final,
    dtRatificacao = dt_ratificacao, dtPublicacao = dt_publicacao,
    pertence14133 = pertence_14133,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta itens de compras sem licitação (módulo legado)
#' @param co_uasg Código da UASG.
#' @param co_orgao Código do órgão.
#' @param dt_ano_aviso_licitacao Ano do aviso de licitação.
#' @param co_conjunto_materiais Código do conjunto de materiais.
#' @param co_servico Código do serviço.
#' @param nu_cpf_cnpj_fornecedor CNPJ/CPF do fornecedor.
#' @inheritParams compras_sem_licitacao
#' @inheritParams itens_licitacao
#' @return Um \code{data.table} com os itens das compras sem licitação.
#' @export
#' @examples
#' \dontrun{itc <- itens_compras_sem_licitacao(dt_ano_aviso_licitacao = 2024,
#'                                             co_uasg = 160194)}
itens_compras_sem_licitacao <- function(co_uasg = NULL, co_orgao = NULL,
                                        dt_ano_aviso_licitacao,
                                        co_modalidade_licitacao = NULL,
                                        co_conjunto_materiais = NULL,
                                        co_servico = NULL,
                                        nu_cpf_cnpj_fornecedor = NULL,
                                        paginar = TRUE,
                                        tamanho_pagina = NULL,
                                        quieto = FALSE) {
  compras_consultar(
    "modulo-legado/6_consultarCompraItensSemLicitacao",
    co_uasg = co_uasg, co_orgao = co_orgao,
    dt_ano_aviso_licitacao = dt_ano_aviso_licitacao,
    co_modalidade_licitacao = co_modalidade_licitacao,
    co_conjunto_materiais = co_conjunto_materiais,
    co_servico = co_servico,
    nu_cpf_cnpj_fornecedor = nu_cpf_cnpj_fornecedor,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta avisos de RDC (CREDENCIAMENTO, Diálogo Competitivo etc.)
#' @param data_publicacao_min,data_publicacao_max Intervalo de publicação.
#' @param ... Demais filtros do endpoint (use
#'   \code{compras_metadados(operacao = "consultarRdc")} para listar).
#' @inheritParams licitacoes
#' @return Um \code{data.table} com os avisos de RDC.
#' @export
#' @examples
#' \dontrun{rdc1 <- rdc(data_publicacao_min = "2024-01-01",
#'                      data_publicacao_max = "2024-01-31")}
rdc <- function(data_publicacao_min, data_publicacao_max, ...,
                paginar = TRUE, tamanho_pagina = NULL, quieto = FALSE) {
  extras <- list(...)
  query <- c(
    list(data_publicacao_min = data_publicacao_min,
         data_publicacao_max = data_publicacao_max),
    extras
  )
  do.call(
    compras_consultar,
    c(list(caminho = "modulo-legado/7_consultarRdc"),
      query,
      list(paginar = paginar, tamanho_pagina = tamanho_pagina,
           quieto = quieto))
  )
}

## Contratações (Lei 14.133, PNCP) ----------------------------------------------

#' Consulta contratações da Lei 14.133/2021
#'
#' Importante: a API exige o filtro \code{codigo_modalidade}; sem ele o
#' endpoint devolve 404 em vez de um erro de validação.
#'
#' @param codigo_modalidade Código da modalidade de compra (obrigatório;
#'   ex.: 6 = pregão eletrônico).
#' @param data_publicacao_pncp_inicial,data_publicacao_pncp_final Intervalo
#'   de publicação no PNCP (\code{Date} ou \code{"YYYY-MM-DD"}).
#' @param unidade_orgao_codigo_unidade Código da unidade do órgão.
#' @param codigo_orgao Código do órgão.
#' @param orgao_entidade_cnpj CNPJ da entidade.
#' @param unidade_orgao_codigo_ibge Código IBGE da unidade.
#' @param unidade_orgao_uf_sigla Sigla da UF da unidade.
#' @param data_atualizacao_pncp Filtro por data de atualização.
#' @param amparo_legal_codigo_pncp Código do amparo legal.
#' @param contratacao_excluida Indica contratação excluída.
#' @param paginar Se \code{TRUE} (padrão), percorre todas as páginas.
#' @param tamanho_pagina Tamanho da página (mínimo 10; padrão da API: 500).
#' @param quieto Se \code{TRUE}, suprime mensagens de progresso.
#' @return Um \code{data.table} com as contratações.
#' @export
#' @examples
#' \dontrun{
#' cont <- contratacoes(codigo_modalidade = 6,
#'                      data_publicacao_pncp_inicial = "2024-01-01",
#'                      data_publicacao_pncp_final = "2024-01-31")
#' }
contratacoes <- function(data_publicacao_pncp_inicial,
                         data_publicacao_pncp_final, codigo_modalidade,
                         unidade_orgao_codigo_unidade = NULL,
                         codigo_orgao = NULL, orgao_entidade_cnpj = NULL,
                         unidade_orgao_codigo_ibge = NULL,
                         unidade_orgao_uf_sigla = NULL,
                         data_atualizacao_pncp = NULL,
                         amparo_legal_codigo_pncp = NULL,
                         contratacao_excluida = NULL, paginar = TRUE,
                         tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-contratacoes/1_consultarContratacoes_PNCP_14133",
    dataPublicacaoPncpInicial = data_publicacao_pncp_inicial,
    dataPublicacaoPncpFinal = data_publicacao_pncp_final,
    codigoModalidade = codigo_modalidade,
    unidadeOrgaoCodigoUnidade = unidade_orgao_codigo_unidade,
    codigoOrgao = codigo_orgao,
    orgaoEntidadeCnpj = orgao_entidade_cnpj,
    unidadeOrgaoCodigoIbge = unidade_orgao_codigo_ibge,
    unidadeOrgaoUfSigla = unidade_orgao_uf_sigla,
    dataAualizacaoPncp = data_atualizacao_pncp,
    amparoLegalCodigoPncp = amparo_legal_codigo_pncp,
    contratacaoExcluida = contratacao_excluida,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta uma contratação por identificador
#'
#' @param tipo Tipo do identificador: \code{"idCompra"} ou
#'   \code{"numeroControlePNCPCompra"}.
#' @param codigo Valor do identificador.
#' @param data_atualizacao_pncp Filtro por data de atualização.
#' @return Um \code{data.table} com a contratação.
#' @export
#' @examples
#' \dontrun{
#' ct <- contratacao_id(tipo = "idCompra", codigo = "15667906002492023")
#' }
contratacao_id <- function(tipo, codigo, data_atualizacao_pncp = NULL) {
  compras_consultar(
    "modulo-contratacoes/1.1_consultarContratacoes_PNCP_14133_Id",
    tipo = tipo, codigo = codigo,
    dataAtualizacaoPncp = data_atualizacao_pncp
  )
}

#' Consulta itens de contratações da Lei 14.133
#' @param data_inclusao_pncp_inicial,data_inclusao_pncp_final Intervalo de
#'   inclusão no PNCP.
#' @param situacao_compra_item Situação do item.
#' @param material_ou_servico Filtra por material ou serviço.
#' @param codigo_classe,codigo_grupo Códigos de classificação do catálogo.
#' @param cod_item_catalogo Código do item de catálogo.
#' @param tem_resultado Indica se há resultado.
#' @param cod_fornecedor Código do fornecedor.
#' @param bps,margem_preferencia_normal Filtros adicionais.
#' @param codigo_ncm,codigo_pdm Códigos NCM e PDM.
#' @param unidade_orgao_codigo_unidade,orgao_entidade_cnpj,data_atualizacao_pncp
#'   Demais filtros.
#' @inheritParams contratacoes
#' @return Um \code{data.table} com os itens das contratações.
#' @export
#' @examples
#' \dontrun{
#' it <- itens_contratacoes(data_inclusao_pncp_inicial = "2024-01-01",
#'                          data_inclusao_pncp_final = "2024-01-31")
#' }
itens_contratacoes <- function(data_inclusao_pncp_inicial,
                               data_inclusao_pncp_final,
                               unidade_orgao_codigo_unidade = NULL,
                               orgao_entidade_cnpj = NULL,
                               situacao_compra_item = NULL,
                               material_ou_servico = NULL,
                               codigo_classe = NULL, codigo_grupo = NULL,
                               cod_item_catalogo = NULL, tem_resultado = NULL,
                               cod_fornecedor = NULL,
                               data_atualizacao_pncp = NULL, bps = NULL,
                               margem_preferencia_normal = NULL,
                               codigo_ncm = NULL, codigo_pdm = NULL,
                               paginar = TRUE, tamanho_pagina = NULL,
                               quieto = FALSE) {
  compras_consultar(
    "modulo-contratacoes/2_consultarItensContratacoes_PNCP_14133",
    dataInclusaoPncpInicial = data_inclusao_pncp_inicial,
    dataInclusaoPncpFinal = data_inclusao_pncp_final,
    unidadeOrgaoCodigoUnidade = unidade_orgao_codigo_unidade,
    orgaoEntidadeCnpj = orgao_entidade_cnpj,
    situacaoCompraItem = situacao_compra_item,
    materialOuServico = material_ou_servico,
    codigoClasse = codigo_classe, codigoGrupo = codigo_grupo,
    codItemCatalogo = cod_item_catalogo, temResultado = tem_resultado,
    codFornecedor = cod_fornecedor,
    dataAtualizacaoPncp = data_atualizacao_pncp, bps = bps,
    margemPreferenciaNormal = margem_preferencia_normal,
    codigoNCM = codigo_ncm, codigoPdm = codigo_pdm,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta o resultado dos itens de contratações da Lei 14.133
#' @param ni_fornecedor CNPJ/CPF do fornecedor.
#' @param codigo_pais Código do país do fornecedor.
#' @param porte_fornecedor_id Porte do fornecedor.
#' @param natureza_juridica_id Natureza jurídica.
#' @param situacao_compra_item_resultado_id Situação do resultado.
#' @param valor_unitario_homologado_inicial,valor_unitario_homologado_final
#'   Intervalo do valor unitário homologado.
#' @param valor_total_homologado_inicial,valor_total_homologado_final
#'   Intervalo do valor total homologado.
#' @param data_resultado_pncp_inicial,data_resultado_pncp_final Intervalo do
#'   resultado no PNCP.
#' @param aplicacao_margem_preferencia Indica aplicação de margem.
#' @param aplicacao_beneficio_meepp Indica benefício MEEPP.
#' @param aplicacao_criterio_desempate Indica critério de desempate.
#' @param orgao_entidade_cnpj CNPJ da entidade.
#' @inheritParams contratacoes
#' @return Um \code{data.table} com os resultados dos itens.
#' @export
#' @examples
#' \dontrun{
#' res <- resultado_itens_contratacoes(
#'   data_resultado_pncp_inicial = "2024-01-01",
#'   data_resultado_pncp_final = "2024-01-31")
#' }
resultado_itens_contratacoes <- function(data_resultado_pncp_inicial,
                                         data_resultado_pncp_final,
                                         unidade_orgao_codigo_unidade = NULL,
                                         orgao_entidade_cnpj = NULL,
                                         ni_fornecedor = NULL,
                                         codigo_pais = NULL,
                                         porte_fornecedor_id = NULL,
                                         natureza_juridica_id = NULL,
                                         situacao_compra_item_resultado_id = NULL,
                                         valor_unitario_homologado_inicial = NULL,
                                         valor_unitario_homologado_final = NULL,
                                         valor_total_homologado_inicial = NULL,
                                         valor_total_homologado_final = NULL,
                                         aplicacao_margem_preferencia = NULL,
                                         aplicacao_beneficio_meepp = NULL,
                                         aplicacao_criterio_desempate = NULL,
                                         paginar = TRUE,
                                         tamanho_pagina = NULL,
                                         quieto = FALSE) {
  compras_consultar(
    "modulo-contratacoes/3_consultarResultadoItensContratacoes_PNCP_14133",
    dataResultadoPncpInicial = data_resultado_pncp_inicial,
    dataResultadoPncpFinal = data_resultado_pncp_final,
    unidadeOrgaoCodigoUnidade = unidade_orgao_codigo_unidade,
    orgaoEntidadeCnpj = orgao_entidade_cnpj,
    niFornecedor = ni_fornecedor, codigoPais = codigo_pais,
    porteFornecedorId = porte_fornecedor_id,
    naturezaJuridicaId = natureza_juridica_id,
    situacaoCompraItemResultadoId = situacao_compra_item_resultado_id,
    valorUnitarioHomologadoInicial = valor_unitario_homologado_inicial,
    valorUnitarioHomologadoFinal = valor_unitario_homologado_final,
    valorTotalHomologadoInicial = valor_total_homologado_inicial,
    valorTotalHomologadoFinal = valor_total_homologado_final,
    aplicacaoMargemPreferencia = aplicacao_margem_preferencia,
    aplicacaoBeneficioMeepp = aplicacao_beneficio_meepp,
    aplicacaoCriterioDesempate = aplicacao_criterio_desempate,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

## Contratos ---------------------------------------------------------------------

#' Consulta contratos
#' @param codigo_orgao Código do órgão (obrigatório).
#' @param data_vigencia_inicial_min,data_vigencia_inicial_max Intervalo do
#'   início da vigência.
#' @param codigo_unidade_gestora Código da unidade gestora.
#' @param codigo_unidade_gestora_origem_contrato Unidade de origem.
#' @param codigo_unidade_realizadora_compra Unidade realizadora.
#' @param numero_contrato Número do contrato.
#' @param codigo_modalidade_compra Modalidade da compra.
#' @param codigo_tipo Tipo do contrato.
#' @param codigo_categoria Categoria do contrato.
#' @param ni_fornecedor CNPJ/CPF do fornecedor.
#' @inheritParams licitacoes
#' @return Um \code{data.table} com os contratos.
#' @export
#' @examples
#' \dontrun{
#' ct <- contratos(codigo_orgao = 81236,
#'                 data_vigencia_inicial_min = "2024-01-01",
#'                 data_vigencia_inicial_max = "2024-06-30")
#' }
contratos <- function(codigo_orgao, data_vigencia_inicial_min,
                      data_vigencia_inicial_max,
                      codigo_unidade_gestora = NULL,
                      codigo_unidade_gestora_origem_contrato = NULL,
                      codigo_unidade_realizadora_compra = NULL,
                      numero_contrato = NULL,
                      codigo_modalidade_compra = NULL,
                      codigo_tipo = NULL, codigo_categoria = NULL,
                      ni_fornecedor = NULL, paginar = TRUE,
                      tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-contratos/1_consultarContratos",
    codigoOrgao = codigo_orgao,
    dataVigenciaInicialMin = data_vigencia_inicial_min,
    dataVigenciaInicialMax = data_vigencia_inicial_max,
    codigoUnidadeGestora = codigo_unidade_gestora,
    codigoUnidadeGestoraOrigemContrato = codigo_unidade_gestora_origem_contrato,
    codigoUnidadeRealizadoraCompra = codigo_unidade_realizadora_compra,
    numeroContrato = numero_contrato,
    codigoModalidadeCompra = codigo_modalidade_compra,
    codigoTipo = codigo_tipo, codigoCategoria = codigo_categoria,
    niFornecedor = ni_fornecedor,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta um contrato por identificador
#' @param tipo Tipo do identificador: \code{"idCompra"} ou
#'   \code{"numeroControlePncpContrato"}.
#' @param codigo Valor do identificador.
#' @return Um \code{data.table} com o contrato.
#' @export
#' @examples
#' \dontrun{ct <- contrato_id(tipo = "idCompra", codigo = "123")}
contrato_id <- function(tipo, codigo) {
  compras_consultar(
    "modulo-contratos/1.1_consultarContratos_Id",
    tipo = tipo, codigo = codigo
  )
}

#' Consulta contratos próximos do fim da vigência
#' @inheritParams contratos
#' @param data_vigencia_final_min,data_vigencia_final_max Intervalo do fim
#'   da vigência.
#' @return Um \code{data.table} com os contratos.
#' @export
#' @examples
#' \dontrun{
#' ctf <- contratos_fim_vigencia(codigo_orgao = 81236,
#'                               data_vigencia_final_min = "2025-01-01",
#'                               data_vigencia_final_max = "2025-12-31")
#' }
contratos_fim_vigencia <- function(codigo_orgao, data_vigencia_final_min,
                                   data_vigencia_final_max,
                                   codigo_unidade_gestora = NULL,
                                   codigo_unidade_gestora_origem_contrato = NULL,
                                   codigo_unidade_realizadora_compra = NULL,
                                   numero_contrato = NULL,
                                   codigo_modalidade_compra = NULL,
                                   codigo_tipo = NULL, codigo_categoria = NULL,
                                   ni_fornecedor = NULL, paginar = TRUE,
                                   tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-contratos/1.2_consultarContratos_FimVigencia",
    codigoOrgao = codigo_orgao,
    dataVigenciaFinalMin = data_vigencia_final_min,
    dataVigenciaFinalMax = data_vigencia_final_max,
    codigoUnidadeGestora = codigo_unidade_gestora,
    codigoUnidadeGestoraOrigemContrato = codigo_unidade_gestora_origem_contrato,
    codigoUnidadeRealizadoraCompra = codigo_unidade_realizadora_compra,
    numeroContrato = numero_contrato,
    codigoModalidadeCompra = codigo_modalidade_compra,
    codigoTipo = codigo_tipo, codigoCategoria = codigo_categoria,
    niFornecedor = ni_fornecedor,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta itens de contratos
#' @inheritParams contratos
#' @param tipo_item Tipo do item.
#' @param codigo_item Código do item de catálogo.
#' @param poder Poder (Ex.: "EXECUTIVO").
#' @param esfera Esfera (F/E/M).
#' @param id_compra Identificador da compra de origem.
#' @return Um \code{data.table} com os itens dos contratos.
#' @export
#' @examples
#' \dontrun{
#' it <- itens_contrato(codigo_orgao = 81236,
#'                      data_vigencia_inicial_min = "2024-01-01",
#'                      data_vigencia_inicial_max = "2024-06-30")
#' }
itens_contrato <- function(codigo_orgao, data_vigencia_inicial_min,
                           data_vigencia_inicial_max,
                           codigo_unidade_gestora = NULL,
                           codigo_unidade_gestora_origem_contrato = NULL,
                           codigo_unidade_realizadora_compra = NULL,
                           numero_contrato = NULL,
                           codigo_modalidade_compra = NULL,
                           tipo_item = NULL, codigo_item = NULL,
                           ni_fornecedor = NULL, poder = NULL, esfera = NULL,
                           id_compra = NULL, paginar = TRUE,
                           tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-contratos/2_consultarContratosItem",
    codigoOrgao = codigo_orgao,
    dataVigenciaInicialMin = data_vigencia_inicial_min,
    dataVigenciaInicialMax = data_vigencia_inicial_max,
    codigoUnidadeGestora = codigo_unidade_gestora,
    codigoUnidadeGestoraOrigemContrato = codigo_unidade_gestora_origem_contrato,
    codigoUnidadeRealizadoraCompra = codigo_unidade_realizadora_compra,
    numeroContrato = numero_contrato,
    codigoModalidadeCompra = codigo_modalidade_compra,
    tipoItem = tipo_item, codigoItem = codigo_item,
    niFornecedor = ni_fornecedor, poder = poder, esfera = esfera,
    idCompra = id_compra,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

## Atas de Registro de Preços (ARP) ----------------------------------------------

#' Consulta atas de registro de preços (ARP)
#' @param data_vigencia_inicial_min,data_vigencia_inicial_max Intervalo do
#'   início da vigência.
#' @param codigo_unidade_gerenciadora Unidade gerenciadora.
#' @param codigo_modalidade_compra Modalidade da compra.
#' @param numero_ata_registro_preco Número da ata.
#' @param data_assinatura_inicial,data_assinatura_final Intervalo de
#'   assinatura.
#' @inheritParams licitacoes
#' @return Um \code{data.table} com as atas.
#' @export
#' @examples
#' \dontrun{
#' atas <- atas_registro_preco(
#'   data_vigencia_inicial_min = "2024-01-01",
#'   data_vigencia_inicial_max = "2024-06-30")
#' }
atas_registro_preco <- function(data_vigencia_inicial_min,
                                data_vigencia_inicial_max,
                                codigo_unidade_gerenciadora = NULL,
                                codigo_modalidade_compra = NULL,
                                numero_ata_registro_preco = NULL,
                                data_assinatura_inicial = NULL,
                                data_assinatura_final = NULL, paginar = TRUE,
                                tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-arp/1_consultarARP",
    dataVigenciaInicialMin = data_vigencia_inicial_min,
    dataVigenciaInicialMax = data_vigencia_inicial_max,
    codigoUnidadeGerenciadora = codigo_unidade_gerenciadora,
    codigoModalidadeCompra = codigo_modalidade_compra,
    numeroAtaRegistroPreco = numero_ata_registro_preco,
    dataAssinaturaInicial = data_assinatura_inicial,
    dataAssinaturaFinal = data_assinatura_final,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

#' Consulta uma ata por número de controle PNCP
#' @param numero_controle_pncp_ata Número de controle PNCP da ata.
#' @param data_atualizacao Filtro por data de atualização.
#' @return Um \code{data.table} com a ata.
#' @export
#' @examples
#' \dontrun{ata <- ata_id("04801221000110-1-000072/2023/72")}
ata_id <- function(numero_controle_pncp_ata, data_atualizacao = NULL) {
  compras_consultar(
    "modulo-arp/1.1_consultarARP_Id",
    numeroControlePncpAta = numero_controle_pncp_ata,
    dataAtualizacao = data_atualizacao
  )
}

#' Consulta itens de atas de registro de preços
#' @inheritParams atas_registro_preco
#' @param numero_item Número do item na ata.
#' @param codigo_item Código do item de catálogo.
#' @param tipo_item Tipo do item.
#' @param ni_fornecedor CNPJ/CPF do fornecedor.
#' @param codigo_pdm Código do PDM.
#' @param numero_compra Número da compra de origem.
#' @return Um \code{data.table} com os itens das atas.
#' @export
#' @examples
#' \dontrun{
#' ita <- itens_ata(data_vigencia_inicial_min = "2024-01-01",
#'                  data_vigencia_inicial_max = "2024-06-30")
#' }
itens_ata <- function(data_vigencia_inicial_min, data_vigencia_inicial_max,
                      codigo_unidade_gerenciadora = NULL,
                      codigo_modalidade_compra = NULL, numero_item = NULL,
                      codigo_item = NULL, tipo_item = NULL,
                      ni_fornecedor = NULL, codigo_pdm = NULL,
                      numero_compra = NULL, data_assinatura_inicial = NULL,
                      data_assinatura_final = NULL, paginar = TRUE,
                      tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-arp/2_consultarARPItem",
    dataVigenciaInicialMin = data_vigencia_inicial_min,
    dataVigenciaInicialMax = data_vigencia_inicial_max,
    codigoUnidadeGerenciadora = codigo_unidade_gerenciadora,
    codigoModalidadeCompra = codigo_modalidade_compra,
    numeroItem = numero_item, codigoItem = codigo_item,
    tipoItem = tipo_item, niFornecedor = ni_fornecedor,
    codigoPdm = codigo_pdm, numeroCompra = numero_compra,
    dataAssinaturaInicial = data_assinatura_inicial,
    dataAssinaturaFinal = data_assinatura_final,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

## Fornecedores -------------------------------------------------------------------

#' Consulta fornecedores
#' @param cnpj CNPJ do fornecedor.
#' @param cpf CPF do fornecedor.
#' @param natureza_juridica_id Código da natureza jurídica.
#' @param porte_empresa_id Código do porte da empresa.
#' @param codigo_cnae Código CNAE.
#' @param ativo Indica fornecedores ativos (obrigatório; padrão
#'   \code{TRUE}).
#' @inheritParams licitacoes
#' @return Um \code{data.table} com os fornecedores.
#' @export
#' @examples
#' \dontrun{f <- fornecedores(cnpj = "00000000000191")}
fornecedores <- function(cnpj = NULL, cpf = NULL,
                         natureza_juridica_id = NULL,
                         porte_empresa_id = NULL, codigo_cnae = NULL,
                         ativo = TRUE, paginar = TRUE,
                         tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-fornecedor/1_consultarFornecedor",
    cnpj = cnpj, cpf = cpf, naturezaJuridicaId = natureza_juridica_id,
    porteEmpresaId = porte_empresa_id, codigoCnae = codigo_cnae,
    ativo = ativo,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

## Indicadores --------------------------------------------------------------------

#' Indicadores consolidados da API de dados abertos
#' @return Um \code{data.table} com os indicadores consolidados.
#' @export
#' @examples
#' \dontrun{indicadores_consolidados()}
indicadores_consolidados <- function() {
  compras_consultar("modulo-indicadores/1_consultarIndicadoresConsolidados")
}

#' Indicadores da API de dados abertos por período
#' @param ano Ano de referência (obrigatório).
#' @param mes Mês de referência.
#' @inheritParams licitacoes
#' @return Um \code{data.table} com os indicadores do período.
#' @export
#' @examples
#' \dontrun{indicadores_periodo(ano = 2025, mes = 6)}
indicadores_periodo <- function(ano, mes = NULL, paginar = TRUE,
                                tamanho_pagina = NULL, quieto = FALSE) {
  compras_consultar(
    "modulo-indicadores/2_consultarIndicadoresPorPeriodo",
    ano = ano, mes = mes,
    paginar = paginar, tamanho_pagina = tamanho_pagina, quieto = quieto
  )
}

## OCDS ---------------------------------------------------------------------------

#' Consulta releases OCDS (Open Contracting Data Standard)
#'
#' @param buyer_id Identificador do comprador.
#' @param release_start_date,release_end_date Intervalo das releases: a API
#'   exige que o período seja de no máximo 1 mês.
#' @param page Página (parâmetro próprio do módulo OCDS).
#' @param offset Deslocamento de registros (parâmetro \code{offSet}).
#' @return Um \code{data.table} com as releases.
#' @export
#' @examples
#' \dontrun{
#' rel <- ocds_releases(buyer_id = "911501",
#'                      release_start_date = "2024-01-01",
#'                      release_end_date = "2024-01-31")
#' }
ocds_releases <- function(buyer_id, release_start_date, release_end_date,
                          page = NULL, offset = NULL) {
  compras_consultar(
    "modulo-ocds/1_releases",
    buyerID = buyer_id, releaseStartDate = release_start_date,
    releaseEndDate = release_end_date, page = page, offSet = offset
  )
}
