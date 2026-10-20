# comprasgovbr

<!-- badges: start -->
<!-- badges: end -->

Acesso programático, direto de R, aos dados abertos das **compras públicas
federais** divulgados pelo Compras.gov.br
(<https://dadosabertos.compras.gov.br>): catálogos de materiais e serviços,
pesquisa de preços praticados, licitações, contratações (Lei 14.133/2021),
contratos, atas de registro de preço, fornecedores e indicadores.

O pacote segue a filosofia do
[`sidra`](https://github.com/rodrigoesborges/sidra): funções simples que
devolvem `data.table`s prontos para análise, paginação automática e uma
**tabela de metadados hierárquica** que documenta todos os endpoints da API.

## Instalação

```r
# development version from GitHub
# install.packages("devtools")
devtools::install_github("distintivelab/comprasgovbr")
```

## Uso

```r
library(comprasgovbr)

# Catálogo de materiais: hierarquia grupo -> classe -> PDM -> item
grupos <- material_grupo()                        # 79 grupos
classes <- material_classe(codigo_grupo = 71)     # mobiliário
pdms <- material_pdm(codigo_classe = 7110)        # mobiliário para escritório
itens <- material_item(codigo_pdm = 313)          # itens já com toda a hierarquia

# Pesquisa de preços praticados: preços agregados por PDM (mobiliário, 2024)
precos_movel <- precos_material(
  tipo = "codigoPdm", codigo = "313",
  data_compra_inicio = "2024-01-01", data_compra_fim = "2024-12-31"
)

# UASGs de um estado (paginacao automatica; 22 mil+ registros em uma chamada)
uasgs_df <- uasgs(sigla_uf = "DF")

# Contratações da Lei 14.133 por modalidade (6 = dispensa)
ctr <- contratacoes(
  codigo_modalidade = 6,
  data_publicacao_pncp_inicial = "2024-01-01",
  data_publicacao_pncp_final = "2024-01-31"
)

# Contratos de um órgão (código + janela de vigência de até 1 ano)
ct <- contratos(
  codigo_orgao = 26237,
  data_vigencia_inicial_min = "2024-01-01",
  data_vigencia_inicial_max = "2024-12-31"
)
```

## Consulta genérica e metadados

Todos os endpoints `GET` da API (mais de 70, em 15 módulos) também estão
disponíveis pela função genérica, com validação de caminho e de parâmetros
obrigatórios feita a partir dos metadados do pacote:

```r
# Descobrir endpoints: tabela hierarquica tag -> modulo -> endpoint -> parametro
compras_metadados(modulo = "modulo-contratos")

# Consultar qualquer endpoint GET
compras_consultar("modulo-material/1_consultarGrupoMaterial")

# Variante CSV (delimitador ";", decimal ",", colunas em snake_case)
compras_consultar("modulo-uasg/1_consultarUasg", sigla_uf = "DF", csv = TRUE)
```

## Funcionamento interno

- **Paginação automática**: a API devolve no máximo 500 registros por página
  (mínimo 10); o pacote percorre todas as páginas e devolve o resultado
  consolidado. Use `paginar = FALSE` para uma página única e `tamanho_pagina`
  para ajustar o tamanho.
- **Erros de conexão viram aviso** (`warning`), não abortam a execução e
  devolvem `NULL`, no mesmo estilo do `sidra`.
- **Datas e lógicos** são normalizados (`Date` -> `"YYYY-MM-DD"`,
  `TRUE` -> `true`) conforme a API espera.
- **Metadados bundled**: `data-raw/compras_api_spec.json` é a especificação
  OpenAPI oficial; `Rscript data-raw/metadados.R` regenera `data/metadados.rda`.

## Requisitos

- R >= 4.1.0
- Pacotes: `httr`, `jsonlite`, `data.table`

## Licença

GPL-3 © [distintivo lab](https://github.com/distintivelab)
