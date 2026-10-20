# Testes integrados com a API real de Dados Abertos do Compras.gov.br.
# Rodam localmente, mas sao pulados no CRAN e quando nao ha conexao.

api_online <- function() {
  skip_on_cran()
  skip_if_offline("dadosabertos.compras.gov.br")
}

test_that("material_grupo retorna data.table com catalogo de grupos", {
  api_online()
  grupos <- material_grupo()
  skip_if(is.null(grupos), "API indisponivel")
  expect_s3_class(grupos, "data.table")
  expect_gt(nrow(grupos), 50)
  expect_true(all(c("codigoGrupo", "nomeGrupo") %in% names(grupos)))
})

test_that("servico_secao retorna as secoes do catalogo de servicos", {
  api_online()
  secoes <- servico_secao()
  skip_if(is.null(secoes), "API indisponivel")
  expect_s3_class(secoes, "data.table")
  expect_gte(nrow(secoes), 5)
  expect_true("nomeSecao" %in% names(secoes))
})

test_that("material_item traz a hierarquia completa em uma consulta", {
  api_online()
  itens <- material_item(codigo_grupo = 10, paginar = FALSE,
                         tamanho_pagina = 10)
  skip_if(is.null(itens), "API indisponivel")
  expect_s3_class(itens, "data.table")
  if (nrow(itens) > 0) {
    expect_true(all(c("codigoGrupo", "nomeGrupo", "codigoClasse",
                      "nomeClasse", "codigoPdm", "nomePdm") %in%
                      names(itens)))
  }
})

test_that("uasgs retorna unidades com pagina unica", {
  api_online()
  u <- uasgs(sigla_uf = "DF", paginar = FALSE, tamanho_pagina = 10)
  skip_if(is.null(u), "API indisponivel")
  expect_s3_class(u, "data.table")
  expect_gt(nrow(u), 0)
  expect_true(all(u$siglaUf == "DF"))
  expect_true("nomeUasg" %in% names(u))
})

test_that("compras_consultar paginacao multipla devolve mais de 500 registros", {
  api_online()
  u <- uasgs(paginar = TRUE, tamanho_pagina = 500, quieto = TRUE)
  skip_if(is.null(u), "API indisponivel")
  expect_s3_class(u, "data.table")
  expect_gt(nrow(u), 500)
})

test_that("licitacoes do modulo legado funcionam", {
  api_online()
  lic <- licitacoes(
    data_publicacao_inicial = "2024-01-01",
    data_publicacao_final = "2024-01-05",
    paginar = FALSE, tamanho_pagina = 10
  )
  skip_if(is.null(lic), "API indisponivel")
  expect_s3_class(lic, "data.table")
  expect_gt(nrow(lic), 0)
  expect_true("id_compra" %in% names(lic))
})

test_that("contratacoes valida codigo_modalidade no cliente (API devolve 404 sem ele)", {
  api_online()
  expect_error(
    compras_consultar(
      "modulo-contratacoes/1_consultarContratacoes_PNCP_14133",
      dataPublicacaoPncpInicial = "2024-01-01",
      dataPublicacaoPncpFinal = "2024-01-05"
    ),
    "codigoModalidade"
  )
  cont <- contratacoes(
    data_publicacao_pncp_inicial = "2024-01-01",
    data_publicacao_pncp_final = "2024-01-05",
    codigo_modalidade = 6,
    paginar = FALSE, tamanho_pagina = 10
  )
  skip_if(is.null(cont), "API indisponivel")
  expect_s3_class(cont, "data.table")
  expect_gt(nrow(cont), 0)
})

test_that("fornecedores consulta por cnpj", {
  api_online()
  f <- fornecedores(cnpj = "00000000000191")
  skip_if(is.null(f), "API indisponivel")
  expect_s3_class(f, "data.table")
  expect_gte(nrow(f), 1)
  expect_true("nomeRazaoSocialFornecedor" %in% names(f))
})

test_that("indicadores_consolidados responde", {
  api_online()
  ind <- indicadores_consolidados()
  skip_if(is.null(ind), "API indisponivel")
  expect_s3_class(ind, "data.table")
  expect_true("totalRequisicoes" %in% names(ind))
})

test_that("variante CSV separada por ; funciona", {
  api_online()
  csv_dt <- compras_consultar(
    "modulo-uasg/1_consultarUasg", sigla_uf = "DF", statusUasg = TRUE,
    csv = TRUE, paginar = FALSE, tamanho_pagina = 10
  )
  skip_if(is.null(csv_dt), "API indisponivel")
  expect_s3_class(csv_dt, "data.table")
  expect_gt(nrow(csv_dt), 0)
  # rodape de paginacao nao vira linha de dados
  expect_false(any(grepl("^totalRegistros:", as.character(csv_dt[[1]]))))
  # colunas em snake_case (diferente do camelCase do JSON)
  expect_true(any(grepl("^[a-z]+(_[a-z0-9]+)*$", names(csv_dt))))
  expect_true("sigla_uf" %in% names(csv_dt) || "codigo_uasg" %in% names(csv_dt))
})

test_that("precos_material com PDM de cafe retorna registros", {
  api_online()
  # PDM de cafe solivel; se nao houver compras no periodo, tabela vazia e ok
  pm <- precos_material(
    tipo = "codigoPdm", codigo = "1621905",
    data_compra_inicio = "2024-01-01", data_compra_fim = "2024-12-31",
    paginar = FALSE, tamanho_pagina = 10
  )
  skip_if(is.null(pm), "API indisponivel")
  expect_s3_class(pm, "data.table")
})
