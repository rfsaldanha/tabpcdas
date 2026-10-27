testthat::test_that("cancelamento e prazo encerram o processo sem publicar resultado", {
  target <- environment(tm_async); original <- tm_launch; last_process <- NULL
  assign("tm_launch",function(config,kind,request=list()) {
    p <- callr::r_bg(function(){Sys.sleep(30);TRUE},supervise=TRUE)
    last_process <<- p
    list(process=p,started=Sys.time(),kind=kind)
  },envir=target)
  on.exit({assign("tm_launch",original,envir=target);if(!is.null(last_process) && last_process$is_alive()) last_process$kill_tree()},add=TRUE)
  config <- tm_config(root);config$timeout <- 0.02
  shiny::testServer(function(input,output,session) {job <- tm_async(session,config)}, {
    job$start("query");testthat::expect_true(last_process$is_alive())
    job$cancel();testthat::expect_false(last_process$is_alive())
    testthat::expect_identical(job$status(),"cancelled")
    testthat::expect_null(job$value())
    job$start("query");Sys.sleep(0.04);session$flushReact()
    testthat::expect_identical(job$status(),"failed")
    testthat::expect_false(last_process$is_alive())
    testthat::expect_null(job$value())
  })
})

testthat::test_that("filtros aguardam um recorte válido sem disparar consultas", {
  f<-fixture()
  shiny::testServer(tm_filter_server,args=list(catalogue=shiny::reactive(f$catalogue),
    context=shiny::reactive(NULL),used=shiny::reactive(character()),config=tm_config(root),on_remove=function(id)NULL),{
    session$setInputs(field="def_sexo",lookup=list(field="def_sexo",ticket=1,text=""))
    testthat::expect_identical(task$status(),"idle")
    testthat::expect_match(output$feedback$html,"Selecione o período e a abrangência",fixed=TRUE)
    testthat::expect_false(touched())
  })
})
