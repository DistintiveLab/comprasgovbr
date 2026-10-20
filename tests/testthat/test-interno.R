# Testes offline da infraestrutura interna do pacote

test_that("metadados vem com a estrutura hierárquica esperada", {
  expect_s3_class(metadados, "data.table")
  cols <- c("tag", "modulo", "caminho", "operacao", "verbo", "sumario",
            "parametro", "em", "obrigatorio", "tipo", "formato", "padrao",
            "minimo", "valores_aceitos", "descricao")
  expect_true(all(cols %in% names(metadados)))
  expect_gt(nrow(metadados), 400)
  expect_gt(uniqueN(metadados$caminho), 70)
  expect_gt(uniqueN(metadados$modulo), 10)
})

test_that("compras_metadados filtra por modulo, tag, operacao e colunas", {
  mats <- compras_metadados(modulo = "modulo-material")
  expect_gt(nrow(mats), 0)
  expect_true(all(mats$modulo == "modulo-material"))

  por_tag <- compras_metadados(modulo = "01 - CATÁLOGO - MATERIAL")
  expect_equal(nrow(por_tag), nrow(mats))

  obrigatorios <- compras_metadados(
    modulo = "modulo-contratacoes", obrigatorio = TRUE
  )
  expect_true(all(obrigatorios$obrigatorio))
  expect_true("codigoModalidade" %in% obrigatorios$parametro)

  expect_warning(compras_metadados(coluna_inexistente = 1), "desconhecida")
})

test_that(".compras_query descarta nulos e normaliza logicais e datas", {
  q <- comprasgovbr:::.compras_query(
    a = 1, b = NULL, c = NA, d = TRUE, e = FALSE,
    f = as.Date("2024-01-15"), g = "texto"
  )
  expect_equal(q, list(a = "1", d = "true", e = "false",
                       f = "2024-01-15", g = "texto"))
  expect_null(comprasgovbr:::.compras_query())
})

test_that("compras_consultar valida caminho e parametros obrigatorios", {
  expect_error(
    compras_consultar("caminho/que/nao/existe"),
    "Caminho desconhecido"
  )
  expect_error(
    compras_consultar("modulo-legado/1_consultarLicitacao"),
    "obrigatorios ausentes"
  )
  # sem tocar a rede: sem data_publicacao_inicial nao passa da validacao
  expect_error(
    compras_consultar("modulo-legado/1_consultarLicitacao",
                      data_publicacao_final = "2024-01-31"),
    "obrigatorios ausentes"
  )
})

test_that("tamanho_pagina abaixo do minimo e rejeitado", {
  skip_if_offline("dadosabertos.compras.gov.br")
  skip_on_cran()
  expect_error(
    compras_consultar("modulo-material/1_consultarGrupoMaterial",
                      tamanho_pagina = 5),
    "minimo"
  )
})

test_that(".compras_get_csv remove o rodape de paginacao", {
  corpo <- paste0(
    "codigo_uasg;nome_uasg;sigla_uf\n",
    "110000;AGENCIA X;DF\n",
    "120000;AGENCIA Y;GO\n",
    "totalRegistros: 1489;totalPaginas: 3;paginasRestantes: 2"
  )
  resposta_falsa <- structure(
    list(
      url = "https://dadosabertos.compras.gov.br/teste",
      status_code = 200,
      content = charToRaw(paste0(corpo, "\n")),
      headers = list(`content-type` = "text/csv")
    ),
    class = "response"
  )
  lido <- comprasgovbr:::.compras_get_csv(
    "modulo-uasg/1_consultarUasg_CSV", paginar = FALSE,
    resposta_stub = resposta_falsa
  )
  skip_if(is.null(lido), "stub nao suportado")
  expect_s3_class(lido, "data.table")
  expect_equal(nrow(lido), 2)
  expect_equal(names(lido), c("codigo_uasg", "nome_uasg", "sigla_uf"))
  expect_false(any(grepl("^totalRegistros:", as.character(lido$codigo_uasg))))
})
