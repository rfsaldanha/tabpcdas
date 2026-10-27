tm_ui <- function() {
  shiny::tagList(
    shiny::tags$head(shiny::tags$link(rel="stylesheet",type="text/css",href="app.css"),shiny::tags$script(src="filters.js")),
    bslib::page_sidebar(
      window_title="tabpcdas",
      title=shiny::div(class="brand",shiny::span(class="brand-mark","t"),
        shiny::span("tab",shiny::strong("pcdas")),shiny::span(class="brand-note","DADOS PÚBLICOS · SAÚDE")),
      theme=bslib::bs_theme(version=5,primary="#126d66",secondary="#476569",bg="#f3f6f5",fg="#203a3b",
                           base_font="system-ui",border_radius="0.65rem"),
      sidebar=bslib::sidebar(width=370,open="desktop",
        shiny::div(class="section-kicker","CONFIGURE SUA CONSULTA"),
        shiny::selectInput("system","1. Sistema de informação",choices=character()),
        shiny::div(class="source-actions",shiny::actionLink("reload","Atualizar bases e períodos")),
        shiny::radioButtons("coverage","2. Abrangência territorial",choices=c("Brasil inteiro"="brazil","Selecionar UFs"="states"),inline=TRUE),
        shiny::conditionalPanel("input.coverage === 'states'",
          shiny::selectizeInput("ufs","UFs",choices=sort(unname(TM_UFS)),multiple=TRUE,
            options=list(placeholder="Selecione ao menos uma UF",plugins=list("remove_button")))),
        shiny::uiOutput("territory_ui"),shiny::uiOutput("period_ui"),
        shiny::tags$hr(),shiny::uiOutput("axes_ui"),
        shiny::div(class="filter-section",shiny::tags$label("7. Filtros (opcional)"),
          shiny::helpText("Valores do mesmo filtro: OU. Filtros diferentes: E."),
          shiny::div(id="filter_cards"),
          shiny::actionButton("add_filter","Adicionar filtro",icon=shiny::icon("plus"),class="btn-outline-secondary w-100")),
        shiny::actionButton("tabulate","Tabular",icon=shiny::icon("table"),class="btn-primary w-100"),
        shiny::div(class="sidebar-footnote","Consultas diretas à PCDaS. Cada tabulação consulta a fonte novamente.")),
      shiny::div(class="page-heading",
        shiny::div(shiny::div(class="section-kicker","EXPLORAR OS DADOS"),
          shiny::h1("Sua pergunta, em uma tabela."),
          shiny::p("Selecione uma base, configure os filtros e exporte os resultados.")),
        shiny::span(class="source-badge","PCDaS · Fiocruz")),
      shiny::uiOutput("job_status"),shiny::uiOutput("availability"),shiny::uiOutput("catalogue_status"),
      bslib::card(class="result-card",full_screen=TRUE,
        bslib::card_header(shiny::div(class="result-header",shiny::strong("Resultado da tabulação"),shiny::uiOutput("downloads"))),
        shiny::uiOutput("result_summary"),shiny::uiOutput("empty_state"),DT::DTOutput("table"),shiny::uiOutput("result_notes")),
      shiny::div(class="page-footer",shiny::span("tabpcdas · Consulta aberta aos dados de saúde"),
        shiny::tags$a(href="https://pcdas.icict.fiocruz.br/",target="_blank",rel="noopener","Sobre a PCDaS")))
  )
}

tm_server <- function(input,output,session,config) {
  catalogue <- shiny::reactiveVal(NULL); result <- shiny::reactiveVal(NULL)
  systems <- shiny::reactiveVal(NULL); cards <- shiny::reactiveVal(list()); serial <- 0L
  metadata <- tm_async(session,config); query_task <- tm_async(session,config)
  meta_kind <- shiny::reactiveVal("systems")
  metadata$start("systems")
  clear_cards <- function() {
    for(id in names(shiny::isolate(cards()))) {
      shiny::isolate(cards())[[id]]$destroy()
      shiny::removeUI(paste0("#",id,"-card"),immediate=TRUE)
    }
    cards(list())
  }
  shiny::observeEvent(metadata$value(),{
    x <- metadata$value()
    if(meta_kind()=="systems") {
      systems(x)
      current <- shiny::isolate(input$system)
      selected <- if(length(current) && current %in% x$id) current else head(x$id,1)
      if(!length(selected)) {catalogue(NULL);clear_cards();query_task$cancel(FALSE)}
      shiny::updateSelectInput(session,"system",choices=stats::setNames(x$id,x$name),selected=selected)
      if(length(current) && identical(current,selected)) {
        meta_kind("catalogue");metadata$start("catalogue",list(id=selected))
      }
    } else if(identical(x$id,shiny::isolate(input$system))) catalogue(x)
  })
  shiny::observeEvent(input$system,{
    shiny::req(nzchar(input$system)); clear_cards(); catalogue(NULL); query_task$cancel(FALSE)
    meta_kind("catalogue"); metadata$start("catalogue",list(id=input$system))
  })
  shiny::observeEvent(input$reload,{
    meta_kind("systems"); metadata$start("systems")
  })
  output$territory_ui <- shiny::renderUI({
    x <- catalogue(); shiny::req(x)
    shiny::selectInput("territory","Referência territorial",choices=stats::setNames(names(x$territories),
      vapply(x$territories,`[[`,character(1),"label")))
  })
  output$period_ui <- shiny::renderUI({
    x <- catalogue(); if(is.null(x)) return(shiny::helpText("Consultando períodos na PCDaS…"))
    old <- shiny::isolate(input$periods); selected <- intersect(old,x$periods)
    shiny::selectizeInput("periods","3. Períodos disponíveis",choices=rev(x$periods),
      selected=if(length(selected)) selected else tail(x$periods,1),multiple=TRUE,
      options=list(placeholder="Selecione um ou mais períodos",plugins=list("remove_button")))
  })
  output$axes_ui <- shiny::renderUI({
    x <- catalogue(); shiny::req(x)
    choices <- stats::setNames(names(x$fields),vapply(x$fields,`[[`,character(1),"label"))
    old <- shiny::isolate(list(row=input$row,column=input$column,enabled=input$column_enabled,measure=input$measure))
    measures <- tm_measures(x$id)
    shiny::tagList(
      shiny::selectizeInput("row","4. Variável de linha",choices=c("Sem discriminação"="__NONE__",choices),
        selected=if((old$row %||% "") %in% c("__NONE__",names(x$fields))) old$row else "TM_UF_RES"),
      shiny::checkboxInput("column_enabled","5. Usar variável de coluna",isTRUE(old$enabled)),
      shiny::conditionalPanel("input.column_enabled",
        shiny::selectizeInput("column","Variável de coluna",choices=choices,
          selected=if((old$column %||% "") %in% names(x$fields)) old$column else if("def_sexo" %in% names(x$fields)) "def_sexo" else "TM_PERIODO")),
      shiny::selectInput("measure","6. Conteúdo",choices=stats::setNames(measures$id,measures$label),
        selected=if((old$measure %||% "") %in% measures$id) old$measure else measures$id[1]))
  })
  context <- shiny::reactive({
    x <- catalogue(); if(is.null(x) || !length(input$periods)) return(NULL)
    ufs <- if(identical(input$coverage,"brazil")) "all" else input$ufs
    if(!length(ufs)) return(NULL)
    tm_query(x$id,input$periods,ufs,territory=input$territory)
  })
  used <- shiny::reactive(vapply(cards(),function(x)x$field(),character(1)))
  remove_card <- function(id) {
    x <- shiny::isolate(cards()); if(is.null(x[[id]])) return()
    x[[id]]$destroy(); x[[id]] <- NULL;cards(x)
    shiny::removeUI(paste0("#",id,"-card"),immediate=TRUE)
  }
  shiny::observeEvent(input$add_filter,{
    if(is.null(catalogue())) return()
    serial <<- serial+1L; id <- paste0("filter",serial)
    shiny::insertUI("#filter_cards",where="beforeEnd",ui=tm_filter_ui(id),immediate=TRUE)
    card <- tm_filter_server(id,catalogue,context,used,config,remove_card)
    x <- cards(); x[[id]] <- card; cards(x)
  })
  current_query <- shiny::reactive({
    x <- catalogue(); if(is.null(x)) tm_abort("Aguarde o catálogo da base.")
    filters <- list()
    for(card in cards()) {
      f <- card$value(); if(!is.null(f$error)) tm_abort(f$error)
      if(f$field %in% names(filters)) tm_abort("Escolha variáveis diferentes nos filtros.")
      filters[[f$field]] <- list(kind=f$kind,value=f$value)
    }
    row <- input$row %||% "__NONE__"; if(row=="__NONE__") row <- ""
    tm_query(x$id,input$periods,if(input$coverage=="brazil") "all" else input$ufs,row,
      if(isTRUE(input$column_enabled)) input$column else "",input$measure,filters,input$territory)
  })
  shiny::observeEvent(input$tabulate,{
    if(query_task$status()=="running") {shiny::showNotification("Aguarde a consulta ou cancele-a.");return()}
    for(card in cards()) card$validate()
    tryCatch({
      q <- current_query(); x <- catalogue(); tm_validate_query(q,x)
      query_task$start("query",list(query=q,catalogue=x))
    },error=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=10))
  })
  shiny::observeEvent(query_task$value(),result(query_task$value()))
  shiny::observeEvent(input$cancel,query_task$cancel())
  output$job_status <- shiny::renderUI({
    q <- query_task$status(); m <- metadata$status()
    running <- q=="running" || m=="running"
    message <- if(q=="running") "Consultando e agregando os dados na PCDaS…" else if(m=="running") "Consultando o catálogo da PCDaS…" else
      if(q %in% c("failed","cancelled")) query_task$error() else if(m=="failed") metadata$error() else NULL
    if(is.null(message)) return(NULL)
    shiny::div(class=paste("job-status",if(!running) "status-error"),role="status",`aria-live`="polite",
      shiny::span(class=if(running) "busy-dot" else "status-dot"),shiny::span(message),
      if(q=="running") shiny::actionButton("cancel","Cancelar",class="btn-sm btn-outline-secondary"))
  })
  output$availability <- shiny::renderUI({
    x <- systems(); if(is.null(x)) return(NULL)
    missing <- setdiff(tm_systems()$name,x$name)
    shiny::div(class="availability-note","SIA e SINAN indisponíveis nesta versão.",
      if(length(missing)) paste("Também sem acesso:",paste(missing,collapse=", ")))
  })
  output$catalogue_status <- shiny::renderUI({
    x <- catalogue(); if(is.null(x)) return(NULL)
    shiny::div(class="catalogue-strip",shiny::strong(x$id),
      shiny::span(paste(min(x$periods),"a",max(x$periods))),shiny::span(paste(length(x$periods),"períodos na PCDaS")),
      shiny::span(paste("Consultado em",format(x$checked_at,"%d/%m/%Y %H:%M"))),
      if(length(x$warnings)) shiny::span(paste(x$warnings,collapse=" ")))
  })
  output$empty_state <- shiny::renderUI({
    if(!is.null(result())) return(NULL)
    shiny::div(class="empty-state",shiny::div(class="table-glyph",shiny::icon("table-cells-large")),
      shiny::h3("Uma tabela começa com uma pergunta"),
      shiny::p("Escolha a base, a abrangência e o período. Selecione a variável de linha e clique em Tabular."))
  })
  output$result_summary <- shiny::renderUI({
    r <- result(); if(is.null(r)) return(NULL)
    q <- tryCatch(current_query(),error=function(e) NULL)
    changed <- is.null(q) || !identical(q,r$query) || !identical(input$system,r$query$system)
    shiny::div(class="result-summary",shiny::h3(paste(r$query$system,"·",r$measure$label)),
      shiny::p(paste("Linha:",r$labels$row,"| Coluna:",r$labels$column)),
      shiny::p(paste("Períodos:",paste(r$query$periods,collapse=", "),"·",r$labels$territory,"· Unidade:",r$measure$unit)),
      if(changed) shiny::div(class="stale-notice","Os controles foram alterados. Clique em Tabular para atualizar. A tabela e os arquivos ainda representam a consulta anterior."))
  })
  output$table <- DT::renderDT({
    r <- result(); shiny::req(r)
    DT::datatable(r$table,rownames=FALSE,escape=TRUE,selection="none",
      options=list(pageLength=25,scrollX=TRUE,order=list(),language=list(
        search="Buscar na tabela:",lengthMenu="Mostrar _MENU_ linhas",info="_START_–_END_ de _TOTAL_ linhas",
        infoEmpty="Nenhuma linha",zeroRecords="Nenhum resultado",paginate=list(previous="Anterior",`next`="Próxima"))))
  },server=TRUE)
  output$result_notes <- shiny::renderUI({
    r <- result(); if(is.null(r)) return(NULL)
    shiny::tags$details(class="result-notes",shiny::tags$summary("Fontes, cobertura e notas da consulta"),
      shiny::p(paste("Gerado em",format(r$created_at,"%d/%m/%Y %H:%M"),"·",nrow(r$provenance),"índice(s) da PCDaS")),
      shiny::tags$ul(lapply(r$warnings,shiny::tags$li)),
      shiny::p("As regras desta ferramenta estão documentadas no projeto. A reprodução exata dos indicadores do TabNet não é presumida."),
      shiny::downloadLink("metadata","Baixar metadados da consulta (JSON)"))
  })
  output$downloads <- shiny::renderUI({
    r <- result(); if(is.null(r)) return(NULL)
    shiny::div(class="download-buttons",shiny::downloadButton("csv","CSV",class="btn-sm"),
      if(tm_excel_ok(r)) shiny::downloadButton("xlsx","Excel",class="btn-sm") else shiny::span("Use CSV: limite do Excel excedido."))
  })
  output$csv <- shiny::downloadHandler(filename=function() paste0(result()$query$system,"-tabulacao.csv"),
    content=function(file) tm_csv(shiny::isolate(result()),file),contentType="text/csv;charset=UTF-8")
  output$xlsx <- shiny::downloadHandler(filename=function() paste0(result()$query$system,"-tabulacao.xlsx"),
    content=function(file) tm_xlsx(shiny::isolate(result()),file),contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  output$metadata <- shiny::downloadHandler(filename=function() paste0(result()$query$system,"-metadados.json"),
    content=function(file) {
      r <- shiny::isolate(result())
      jsonlite::write_json(list(consulta=r$query,metadados=tm_metadata(r),
        fontes=r$provenance[,setdiff(names(r$provenance),c("dbc_path","data_path")),drop=FALSE]),file,
        pretty=TRUE,auto_unbox=TRUE,na="null")
    },contentType="application/json")
}
