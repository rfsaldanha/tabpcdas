# Processes and results belong only to their Shiny session. callr removes its
# transport files on collection; there is no persistent queue or result cache.
tm_launch <- function(config,kind,request=list()) {
  process <- callr::r_bg(function(config,kind,request) {
    setwd(config$root)
    source("R/bootstrap.R",local=TRUE); tm_load(config$root,environment())
    switch(kind,systems=tm_available(),catalogue=tm_catalogue(request$id,config),
      query=tm_run_query(request$query,request$catalogue,config),
      values={answer <- tm_values(request$catalogue,request$query,request$field,request$search);
        answer$ticket <- request$ticket;answer},
      stop("Operação inválida."))
  },args=list(config=config,kind=kind,request=request),libpath=.libPaths(),
    env=c(pcdas_token=Sys.getenv("pcdas_token")),supervise=TRUE,stdout="|",stderr="|")
  list(process=process,started=Sys.time(),kind=kind)
}

tm_async <- function(session,config) {
  task <- shiny::reactiveVal(NULL); value <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("idle"); error <- shiny::reactiveVal(NULL)
  cancel <- function(message=TRUE) {
    x <- shiny::isolate(task())
    if(!is.null(x)) {if(x$process$is_alive()) {x$process$kill_tree(); x$process$wait(timeout=1000)}; task(NULL)}
    if(message) {status("cancelled"); error("Consulta cancelada.")}
    else {status("idle");error(NULL);value(NULL)}
  }
  start <- function(kind,request=list()) {
    cancel(FALSE); value(NULL); error(NULL); status("running")
    task(tm_launch(config,kind,request))
  }
  poll <- shiny::observe({
    shiny::invalidateLater(200,session)
    x <- task(); if(is.null(x)) return()
    if(as.numeric(difftime(Sys.time(),x$started,units="secs"))>config$timeout) {
      cancel(FALSE); status("failed"); error("A consulta excedeu o prazo. Reduza os períodos ou aplique filtros."); return()
    }
    if(!x$process$is_alive()) {
      answer <- tryCatch(x$process$get_result(),error=function(e) e)
      task(NULL)
      if(inherits(answer,"error")) {
        status("failed")
        msg <- conditionMessage(answer)
        # Never expose request bodies or tokens returned by transport errors.
        error(if(grepl("Credencial|índices autorizados|PCDaS|Consulta acima",msg))
          sub("^.*?\\n", "",msg) else "Não foi possível concluir a consulta. Tente novamente.")
      } else {value(answer); status("complete")}
    }
  })
  session$onSessionEnded(function() cancel(FALSE))
  list(start=start,cancel=cancel,value=value,status=status,error=error,
       destroy=function(){cancel(FALSE);poll$destroy()})
}
