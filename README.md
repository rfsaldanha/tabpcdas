# tabpcdas

Aplicativo Shiny para tabular dados de saúde e população com consultas agregadas à
**PCDaS**, por meio do [rpcdas](https://rfsaldanha.github.io/rpcdas/). Exporta CSV
UTF-8 e Excel com metadados e fonte. Não baixa microdados nem mantém cache de dados,
catálogos ou resultados entre sessões.

Repositório: <https://github.com/rfsaldanha/tabpcdas>.

## Executar

Requer R >= 4.3 e uma credencial da PCDaS autorizada para os índices usados.
Configure `pcdas_token` no ambiente do processo ou em `.Renviron` (fora do Git).
Não inclua a credencial no código, no navegador ou em logs.

```r
install.packages("renv")
renv::restore()
shiny::runApp()
```

O rpcdas está fixado no commit `a74a50a6c5396b79d8045927d7761e9f0be4b0b0`
(0.4.1). `renv.lock` inclui as dependências de execução e testes.
`TABPCDAS_TIMEOUT` controla o prazo total de cada operação (240 segundos
por padrão). Execuções são isoladas em processos filhos supervisionados por sessão;
cancelar ou fechar a sessão encerra esses processos. Resultados permanecem apenas
na sessão, e o transporte temporário do callr é liberado após sua coleta.

## Bases e semântica

| Base | Índice | Período | Conteúdo |
| --- | --- | --- | --- |
| SIM | `datasus-sim` | Ano do óbito | Óbitos |
| SIM fetal | `datasus-sim-dofet` | Ano do óbito | Óbitos fetais |
| SINASC | `datasus-sinasc` | Ano do nascimento | Nascidos vivos |
| SIH | `datasus-sih` | Competência | AIHs, valor total, dias de permanência |
| CNES | `datasus-cnes` | Competência | Estabelecimentos distintos |
| População | `datasus-pop` | Ano de referência | População residente |

A lista é intersectada com os índices autorizados para a credencial do servidor.
SIA e SINAN não estão disponíveis nesta versão. Os períodos vêm de agrupamentos
remotos dos campos de referência, sem preencher lacunas ou presumir atualização.
O índice populacional acessível fornece apenas totais municipais anuais, sem sexo
ou idade. As séries da PCDaS podem diferir de outras versões publicadas pelo DataSUS.

Brasil inteiro e seleção de UFs são mutuamente exclusivos. A referência padrão é
residência; SIM, SINASC e SIH também permitem ocorrência, nascimento ou atendimento.
CNES usa localização do estabelecimento e população usa território de referência.
Brasil inclui os registros sem território identificado; uma seleção de UFs só inclui
os registros correspondentes. As variáveis dos eixos mantêm o significado expresso
em seus rótulos, independentemente da referência territorial do recorte.

A coluna é opcional. Os filtros usam cartões com seleção de variável e controles
adequados ao tipo. Valores de um filtro são combinados por OU, e filtros diferentes
por E. Categorias são buscadas no índice para o período e a abrangência escolhidos;
a busca não depende dos demais filtros. Até 200 opções são apresentadas por busca,
com aviso para refinar o texto. A busca ocorre no próprio seletor, após uma pausa
de 600 ms na digitação; respostas de buscas anteriores são descartadas. Os valores
selecionados permanecem disponíveis, inclusive após atualizar o catálogo.
Filtros incompletos impedem a tabulação; mensagens de preenchimento aparecem após
interação ou tentativa de tabular. Intervalos de datas incluem o dia final.

Os campos descritivos da PCDaS são priorizados. Os nomes técnicos permanecem
visíveis para identificação; campos de identificação pessoal e campos sem suporte
à agregação não são oferecidos. Nomes municipais vêm da referência presente no
índice, com o código preservado como texto, sem acrescentar dígitos. Código e nome
são consultados em campos separados e exportados em colunas distintas. Quando
município é a variável de coluna, o resultado usa formato longo, com código, nome
e valor em colunas próprias. Municípios nos dois eixos têm títulos que identificam
a perspectiva (residência, ocorrência, nascimento ou atendimento). Totais ficam
com código vazio, e identificações ausentes permanecem ausentes. Não se presume
reconstrução dos limites históricos.

## Agregação e precisão

Filtros, somas e agrupamentos são executados na PCDaS. O Shiny formata a tabela e
as exportações. Cada tabulação faz novas chamadas; alterações nos controles não
modificam o resultado já exibido até a próxima tabulação.

O conversor público de `generic_pcdas_query()` na versão fixada elimina células
JSON nulas. O adaptador usa o mesmo `pcdas_query_request` paginado do rpcdas e um
decodificador que preserva nulos e tipos, incluindo páginas de cursor sem esquema.
Essa interface interna tem teste de assinatura e exige revisão ao atualizar rpcdas.

`COUNT(DISTINCT)` no Elasticsearch usa cardinalidade aproximada. Para CNES, a
consulta agrupa exatamente por estabelecimento e dimensões na fonte; o aplicativo
conta esses grupos únicos. Cada margem é calculada separadamente. Consultas CNES
amplas podem levar mais tempo e atingir o prazo de execução.

Somas com valores ausentes mantêm as células e totais afetados como `NA`. Ausência
de registros e conteúdo ausente não são confundidos. CNES e população permanecem
separados por período, sem total acumulado entre competências/anos. Resultados acima
de dois milhões de grupos são rejeitados; tabelas acima de 500 colunas ficam em
formato longo. Exportações respeitam os limites de linhas e colunas do Excel.

## Verificação

```sh
Rscript scripts/test.R
node tests/filters-js.test.cjs
Rscript scripts/smoke-test.R /tmp/pcdas-smoke.json
```

Testes locais usam respostas agregadas reais e pequenas, gravadas em
`tests/fixtures` com origem e horário. A integração exige `pcdas_token`: verifica
as seis bases, totais independentes, somas, períodos, paginação, busca municipal,
valores ausentes, datas inclusivas e consulta sem resultados. Nenhum teste usa dados
de saúde inventados. Regravar fixtures é uma ação explícita documentada na pasta.

## Publicação nativa na NXCTIC009

- URL: <https://shiny.icict.fiocruz.br/tabpcdas/>
- Aplicativo: `/dados/htdocs/shiny.icict.fiocruz.br/tabpcdas`
- Biblioteca isolada: `/dados/tabpcdas/library-pcdas`
- Cache de instalação de pacotes renv: `/dados/tabpcdas/renv-cache`
- Usuário de execução: `shiny`.

A publicação copia arquivos para staging, restaura o lockfile, executa testes e
substitui o diretório do aplicativo, preservando uma cópia anterior para reversão.
Não usa Docker nem reinicia os outros aplicativos. O `.Renviron` do servidor tem
permissão restrita, contém `pcdas_token`, os caminhos renv e o prazo de execução.
Toque `restart.txt` apenas neste aplicativo depois da substituição.
O endereço antigo `/tabmicrodatasus/` redireciona para `/tabpcdas/`. Um link
compatível de `/dados/tabmicrodatasus` para `/dados/tabpcdas` preserva os caminhos
das bibliotecas e das versões anteriores usadas em reversão.

As variáveis antigas `TABMICRODATASUS_DATA_DIR`, `TABMICRODATASUS_WORKERS` e
`TABMICRODATASUS_QUERY_MEMORY` não são usadas. A antiga fila SQLite, trabalhadores
persistentes, downloads e partições deixam de participar da execução. O diretório
antigo de dados é conservado somente para reversão da migração e pode ser removido
depois da aceitação da publicação. Nenhuma migração de seus resultados é necessária.

Monitorar erros e duração pelo log deste aplicativo no Shiny Server. Dependências
podem ser instaladas em uma biblioteca nova durante staging para manter a versão
anterior utilizável. A versão de R do servidor (4.5.2) difere do desenvolvimento
(4.3.3); ambos executam as verificações antes da publicação.
