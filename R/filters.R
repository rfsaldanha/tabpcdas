tm_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(id=ns("card"),class="filter-card",
    shiny::div(class="filter-card-heading",shiny::strong("Filtro"),
      shiny::actionButton(ns("remove"),"Remover",class="btn-sm btn-outline-secondary")),
    shiny::selectizeInput(ns("field"),"Filtrar por",choices=c("Escolha uma variável"=""),
      options=list(placeholder="Escolha o que deseja filtrar")),
    shiny::uiOutput(ns("controls")),shiny::uiOutput(ns("feedback")))
}

tm_filter_server <- function(id,catalogue,context,used,config,on_remove) {
  shiny::moduleServer(id,function(input,output,session) {
    task <- tm_async(session,config);observers <- list()
    touched <- shiny::reactiveVal(FALSE);option_labels <- shiny::reactiveVal(character())
    selected <- shiny::reactive(input$field %||% "")
    observers[[1]] <- shiny::observe({
      x <- catalogue(); shiny::req(x)
      fields <- x$fields[!names(x$fields) %in% setdiff(used(),input$field)]
      choices <- stats::setNames(names(fields),vapply(fields,`[[`,character(1),"label"))
      shiny::updateSelectizeInput(session,"field",choices=c("Escolha uma variável"="",choices),
        selected=shiny::isolate(input$field %||% ""),server=FALSE)
    })
    rendered_field <- NULL
    output$controls <- shiny::renderUI({
      x <- catalogue(); field <- input$field; shiny::req(x,nzchar(field %||% ""))
      f <- x$fields[[field]]; shiny::req(f)
      ns <- session$ns
      previous <- shiny::isolate(if(identical(rendered_field,field))
        list(value=input$value,min=input$min,max=input$max,start=input$start,end=input$end) else list())
      previous$value <- previous$value %||% character()
      rendered_field <<- field
      quote_js <- function(x) as.character(jsonlite::toJSON(x,auto_unbox=TRUE))
      if(f$kind=="category") {
        labels <- shiny::isolate(option_labels())[previous$value]
        labels[is.na(labels)] <- previous$value[is.na(labels)]
        choices <- c(stats::setNames(TM_MISSING,"Não informado"),stats::setNames(previous$value,labels))
        controls <- shiny::selectizeInput(ns("value"),
          if(isTRUE(f$municipality)) "Selecione os municípios" else "Selecione uma ou mais opções",
          choices=choices[!duplicated(choices)],selected=previous$value,multiple=TRUE,
          options=list(placeholder=if(isTRUE(f$municipality)) "Digite o nome ou código" else "Digite para buscar opções",
            plugins=list("remove_button"),valueField="value",labelField="label",searchField=c("label","value"),
            loadThrottle=600,preload=TRUE,
            load=I(paste0("function(query, callback) { window.tmFilterLoad(this, ",quote_js(ns("lookup")),", ",quote_js(field),", query, callback); }")),
            onType=I("function(text) { window.tmFilterType(this, text); }")))
      } else if(f$kind=="number") controls <- shiny::div(class="filter-range",
        shiny::numericInput(ns("min"),"Mínimo",value=previous$min %||% NA),
        shiny::numericInput(ns("max"),"Máximo",value=previous$max %||% NA))
      else controls <- shiny::tagList(shiny::dateInput(ns("start"),"De",value=previous$start %||% NA,language="pt-BR",format="dd/mm/yyyy"),
        shiny::dateInput(ns("end"),"Até",value=previous$end %||% NA,language="pt-BR",format="dd/mm/yyyy"))
      shiny::div(onfocusout=paste0("Shiny.setInputValue(",quote_js(ns("touched")),", Date.now(), {priority:'event'});"),controls)
    })
    selection <- shiny::reactive({
      field <- input$field %||% ""; x <- catalogue()
      if(!nzchar(field) || is.null(x$fields[[field]])) return(list(field="",error="Escolha o que deseja filtrar."))
      f <- x$fields[[field]]
      value <- switch(f$kind,category=input$value,number=c(input$min,input$max),date=c(input$start,input$end))
      error <- NULL
      if(!length(value) || anyNA(value)) error <- "Selecione uma ou mais opções."
      if(f$kind!="category" && (length(value)!=2 || anyNA(value) || value[1]>value[2])) error <- "Informe um intervalo válido."
      list(field=field,kind=f$kind,value=as.character(value),error=error)
    })
    observers[[2]] <- shiny::observeEvent(input$lookup,{
      lookup <- input$lookup;x <- catalogue();query <- context()
      task$cancel(FALSE)
      if(is.null(x) || !identical(lookup$field,input$field)) return()
      if(is.null(query)) {
        session$sendCustomMessage("tm-filter-options",list(id=session$ns("value"),ticket=lookup$ticket,options=list()))
        return()
      }
      if(x$fields[[input$field]]$kind!="category") return()
      task$start("values",list(catalogue=x,query=query,field=input$field,
        search=lookup$text,ticket=lookup$ticket))
    })
    observers[[3]] <- shiny::observeEvent(task$value(),{
      x <- task$value();lookup <- input$lookup
      if(!identical(x$ticket,lookup$ticket) || !identical(lookup$field,input$field)) return()
      options <- rbind(data.frame(value=TM_MISSING,label="Não informado"),x$options)
      labels <- option_labels();labels[options$value] <- options$label;option_labels(labels)
      session$sendCustomMessage("tm-filter-options",list(id=session$ns("value"),ticket=x$ticket,
        options=lapply(seq_len(nrow(options)),function(i) as.list(options[i,,drop=FALSE]))))
    })
    observers[[4]] <- shiny::observeEvent(input$remove,on_remove(id))
    observers[[5]] <- shiny::observeEvent(input$touched,touched(TRUE))
    observers[[6]] <- shiny::observeEvent(input$field,{
      touched(FALSE);option_labels(character());task$cancel(FALSE)
    },priority=10)
    observers[[7]] <- shiny::observeEvent(context(),{
      task$cancel(FALSE)
      session$sendCustomMessage("tm-filter-reload",list(id=session$ns("value")))
    },ignoreInit=TRUE)
    observers[[8]] <- shiny::observeEvent(task$status(),{
      if(task$status() %in% c("failed","cancelled") && !is.null(input$lookup))
        session$sendCustomMessage("tm-filter-options",list(id=session$ns("value"),ticket=input$lookup$ticket,failed=TRUE))
    })
    output$feedback <- shiny::renderUI({
      state <- task$status();f <- selection();response <- task$value()
      if(is.null(context())) return(shiny::div(class="filter-feedback","Selecione o período e a abrangência para ver as opções."))
      shiny::tagList(
        if(state=="running") shiny::div(class="filter-feedback",role="status","Carregando opções…"),
        if(state=="failed") shiny::div(class="filter-error",role="alert","Não foi possível carregar as opções. Tente buscar novamente."),
        if(state=="complete" && isTRUE(response$more)) shiny::div(class="filter-feedback","Digite mais para refinar a busca."),
        if(state=="complete" && !length(response$values)) shiny::div(class="filter-feedback","Nenhuma opção encontrada."),
        if(touched() && !is.null(f$error)) shiny::div(class="filter-error",f$error))
    })
    list(field=selected,value=selection,validate=function()touched(TRUE),
      destroy=function(){task$destroy();lapply(observers,function(x)x$destroy())})
  })
}
