testthat::test_that("o adaptador usa a API paginada do rpcdas fixado", {
  f <- getFromNamespace("pcdas_query_request","rpcdas")
  testthat::expect_true(all(c("body","pcdas_token","max_tries") %in% names(formals(f))))
  testthat::expect_identical(as.character(utils::packageVersion("rpcdas")),"0.4.1")
})

testthat::test_that("abrangência e filtros inválidos não ampliam uma consulta", {
  f <- fixture();q <- f$query;c <- f$catalogue
  testthat::expect_error(tm_query(c$id,"2022",c("all","AC")),"simultaneamente")
  q$ufs <- character();testthat::expect_error(tm_validate_query(q,c),"ao menos uma UF")
  q <- f$query;q$column <- q$row;testthat::expect_error(tm_validate_query(q,c),"diferentes")
  q <- f$query;q$periods <- "1800";testthat::expect_error(tm_validate_query(q,c),"períodos disponíveis")
  q <- f$query;q$filters <- list(idade_obito_anos=list(kind="number",value=c(30,10)))
  testthat::expect_error(tm_validate_query(q,c),"Intervalo inválido")
  q$filters <- list(def_sexo=list(kind="category",value=character()))
  testthat::expect_error(tm_validate_query(q,c),"Preencha")
})

testthat::test_that("SQL usa campos permitidos, filtros combinados e datas inclusivas", {
  f <- fixture();q <- f$query
  q$filters <- list(def_sexo=list(kind="category",value=c("Feminino",TM_MISSING)),
    CAUSABAS=list(kind="category",value="' OR TRUE --"),data_obito=list(kind="date",value=c("2022-01-01","2022-01-31")))
  testthat::expect_no_error(tm_validate_query(q,f$catalogue))
  sql <- tm_where(q,f$catalogue)
  testthat::expect_match(sql,"IS NULL OR",fixed=TRUE)
  testthat::expect_match(sql,"''' OR TRUE --'",fixed=TRUE)
  testthat::expect_match(sql,"2022-02-01",fixed=TRUE)
  testthat::expect_match(sql,'"ano_obito" IN (2022)',fixed=TRUE)
  q$row <- 'bad"; DELETE';testthat::expect_error(tm_validate_query(q,f$catalogue),"inválida")
})

testthat::test_that("agregados reais da PCDaS mantêm células, totais e exportações", {
  f <- fixture(); original <- tm_sql; target <- environment(tm_run_query)
  assign("tm_sql",function(sql,...) {x <- f$responses[[sql]];if(is.null(x)) stop("Consulta não gravada");x},envir=target)
  on.exit(assign("tm_sql",original,envir=target))
  r <- tm_run_query(f$query,f$catalogue,tm_config(root))
  testthat::expect_equal(tail(r$table$Total,1),4159)
  testthat::expect_equal(r$table$Total[r$table$Linha=="Feminino"],1722)
  testthat::expect_equal(sum(r$long$valor[!r$long$row_total & !r$long$col_total]),4159)
  q <- f$query;q$column <- ""
  row <- tm_run_query(q,f$catalogue,tm_config(root))
  testthat::expect_identical(names(row$table),c("Linha","Total"))
  testthat::expect_equal(row$table$Total[row$table$Linha=="Feminino"],1722)
  csv <- tempfile(fileext=".csv");xlsx <- tempfile(fileext=".xlsx")
  on.exit(unlink(c(csv,xlsx)),add=TRUE)
  tm_csv(row,csv);tm_xlsx(row,xlsx)
  testthat::expect_equal(utils::read.csv2(csv,fileEncoding="UTF-8-BOM")$Total,row$table$Total)
  testthat::expect_equal(readxl::read_excel(xlsx)$Total,row$table$Total)
  testthat::expect_identical(readxl::excel_sheets(xlsx),c("Tabela","Metadados","Fontes"))
  testthat::expect_true("rpcdas" %in% tm_metadata(row)$Item)
})

testthat::test_that("paginação e ausentes da fonte são preservados", {
  f <- readRDS(file.path(root,"tests/fixtures/paginated-null.rds"))
  testthat::expect_gt(length(f$pages),1L)
  x <- tm_decode_pages(f$pages)
  testthat::expect_true(anyNA(x))
  testthat::expect_equal(nrow(x),sum(vapply(f$pages,function(p)length(p$rows),integer(1))))
  testthat::expect_type(x$n,"double")
  testthat::expect_equal(tm_decode_pages(list()),data.frame())
  broken <- f$pages; broken[[1]]$rows[[1]] <- broken[[1]]$rows[[1]][-1]
  testthat::expect_error(tm_decode_pages(broken),"incompleta")
})

testthat::test_that("estoques conservam o período e CNES usa grupos exatos", {
  f <- fixture(); c <- f$catalogue;c$id <- "CNES-ST";c$index <- "datasus-cnes"
  c$period_columns <- "COMPETEN";c$fields$TM_PERIODO$expr <- '"COMPETEN"'
  c$territories <- list(location=list(expr='"uf_SIGLA_UF.keyword"',label="Localização"))
  q <- tm_query("CNES-ST","2025-07","all",row="def_sexo")
  sql <- tm_aggregate_sql(q,c,"def_sexo","def_sexo","")
  testthat::expect_match(sql,"GROUP BY periodo, linha, entidade",fixed=TRUE)
  testthat::expect_false(grepl("COUNT(DISTINCT",sql,fixed=TRUE))
  testthat::expect_match(sql,'"COMPETEN" IN (\'202507\')',fixed=TRUE)
})
