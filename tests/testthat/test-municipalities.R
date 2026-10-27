municipal_fixture <- function() readRDS(file.path(root,"tests/fixtures/municipalities.rds"))
with_recorded <- function(f,code) {
  target <- environment(tm_run_query);original <- tm_sql
  assign("tm_sql",function(sql,...) {x<-f$responses[[sql]];if(is.null(x)) stop("Consulta não gravada");x},envir=target)
  on.exit(assign("tm_sql",original,envir=target))
  force(code)
}

testthat::test_that("municípios são consultados em campos distintos e homônimos não se fundem", {
  f<-municipal_fixture()
  with_recorded(f,{
    r<-tm_run_query(f$queries$row,f$catalogue,tm_config(root))
    testthat::expect_identical(names(r$table)[1:2],c("Código do município","Município"))
    testthat::expect_type(r$table[[1]],"character")
    testthat::expect_equal(tail(r$table$Total,1),4159)
    testthat::expect_identical(tail(r$table[[1]],1),"")
    testthat::expect_equal(sum(head(r$table$Total,-1)),4159)
    testthat::expect_false(any(grepl(" — ",r$table$Município,fixed=TRUE)))
    testthat::expect_false(grepl("CONCAT",r$sql[1],fixed=TRUE))
    homonyms<-tm_run_query(f$queries$homonyms,f$catalogue,tm_config(root))$table
    testthat::expect_equal(sum(homonyms$Município=="Bom Jesus"),5)
    testthat::expect_equal(length(unique(head(homonyms[[1]],-1))),5)
    testthat::expect_equal(tail(homonyms$Total,1),388)
  })
})

testthat::test_that("municípios em colunas e nos dois eixos usam formato longo", {
  f<-municipal_fixture()
  with_recorded(f,{
    r<-tm_run_query(f$queries$col,f$catalogue,tm_config(root))
    testthat::expect_identical(names(r$table),c("Linha","Código do município","Município","Valor"))
    testthat::expect_equal(r$table$Valor[r$table$Linha=="Total" & r$table$Município=="Total"],4159)
    both<-tm_run_query(f$queries$both,f$catalogue,tm_config(root))
    testthat::expect_identical(names(both$table),c("Código do município de residência","Município de residência",
      "Código do município de ocorrência","Município de ocorrência","Valor"))
    testthat::expect_equal(both$table$Valor[both$table[[2]]=="Total" & both$table[[4]]=="Total"],4159)
    testthat::expect_equal(sum(both$long$valor[!both$long$row_total & !both$long$col_total]),4159)
  })
})

testthat::test_that("códigos municipais permanecem texto em CSV e Excel", {
  f<-municipal_fixture()
  with_recorded(f,{
    for(q in f$queries[c("row","col","both")]) {
      r<-tm_run_query(q,f$catalogue,tm_config(root))
      csv<-tempfile(fileext=".csv");xlsx<-tempfile(fileext=".xlsx")
      on.exit(unlink(c(csv,xlsx)),add=TRUE)
      tm_csv(r,csv);tm_xlsx(r,xlsx)
      text<-utils::read.csv2(csv,fileEncoding="UTF-8-BOM",colClasses="character",check.names=FALSE)
      excel<-readxl::read_excel(xlsx)
      testthat::expect_identical(names(text),names(r$table))
      testthat::expect_identical(names(excel),names(r$table))
      for(name in names(r$table)[startsWith(names(r$table),"Código")]) {
        testthat::expect_identical(text[[name]],r$table[[name]])
        testthat::expect_type(excel[[name]],"character")
        testthat::expect_identical(excel[[name]][nzchar(r$table[[name]])],r$table[[name]][nzchar(r$table[[name]])])
      }
    }
  })
})

testthat::test_that("ausência de identificação não produz código ou nome inventado", {
  f<-municipal_fixture()
  with_recorded(f,{
    r<-tm_run_query(f$queries$row,f$catalogue,tm_config(root))
    # Fault injection: remove identifying values from a real aggregate;
    # counts and rows are unchanged and no synthetic health records are created.
    long<-r$long;pick<-which(!long$row_total)[1]
    code<-long$linha[pick]
    long$linha[long$linha==code]<-NA_character_
    long$linha_nome[is.na(long$linha)]<-NA_character_
    out<-tm_wide(long,FALSE,f$catalogue$fields$TM_MUN_RES,f$catalogue$fields$def_sexo)
    testthat::expect_true(anyNA(out[[1]]))
    testthat::expect_true(all(out$Município[is.na(out[[1]])]=="Não informado"))
    testthat::expect_equal(tail(out$Total,1),4159)
  })
})

testthat::test_that("municípios preservam anos e competências sem totalizar estoques", {
  fixtures<-readRDS(file.path(root,"tests/fixtures/municipal-stocks.rds"))$cases
  for(f in fixtures) with_recorded(f,{
    r<-tm_run_query(f$query,f$catalogue,tm_config(root))
    periods<-f$query$periods
    testthat::expect_true(all(periods %in% names(r$table)))
    testthat::expect_false("Total" %in% names(r$table))
    testthat::expect_identical(names(r$table)[1:2],c("Código do município","Município"))
    for(period in periods) testthat::expect_equal(sum(head(r$table[[period]],-1)),tail(r$table[[period]],1))
    col<-tm_run_query(f$column_query,f$catalogue,tm_config(root))$table
    testthat::expect_equal(sort(unique(col$Período)),sort(periods))
    for(period in periods) testthat::expect_equal(col$Valor[col$Período==period & col$Município=="Total"],tail(r$table[[period]],1))
  })
})
