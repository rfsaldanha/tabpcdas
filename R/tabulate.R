tm_query <- function(system,periods,ufs,row="",column="",measure=NULL,filters=list(),territory=NULL) {
  if("all" %in% ufs && length(unique(ufs))>1L) tm_abort("Brasil e UFs não podem ser selecionados simultaneamente.")
  list(system=system,periods=sort(unique(as.character(periods))),
    ufs=if(identical(as.character(ufs),"all")) sort(unname(TM_UFS)) else sort(unique(ufs)),
    row=row %||% "",column=column %||% "",measure=measure %||% tm_measures(system)$id[[1]],
    filters=filters,territory=territory %||% if(system %in% c("CNES-ST","POP-PCDAS")) "location" else "residence")
}

tm_validate_query <- function(query,catalogue) {
  if(!identical(query$system,catalogue$id)) tm_abort("O catálogo não corresponde à base selecionada.")
  if(!length(query$periods) || any(!query$periods %in% catalogue$periods)) tm_abort("Selecione períodos disponíveis.")
  if(!length(query$ufs) || any(!query$ufs %in% unname(TM_UFS))) tm_abort("Selecione ao menos uma UF válida.")
  if(!query$territory %in% names(catalogue$territories)) tm_abort("Perspectiva territorial inválida.")
  dimensions <- c(query$row,query$column); dimensions <- dimensions[nzchar(dimensions)]
  if(anyDuplicated(dimensions)) tm_abort("Escolha variáveis diferentes para linha e coluna.")
  if(any(!dimensions %in% names(catalogue$fields))) tm_abort("Variável de tabulação inválida.")
  if(!query$measure %in% tm_measures(query$system)$id) tm_abort("Conteúdo inválido para esta base.")
  if(anyDuplicated(names(query$filters))) tm_abort("Escolha variáveis diferentes nos filtros.")
  if(any(!names(query$filters) %in% names(catalogue$fields))) tm_abort("Filtro inválido.")
  for(name in names(query$filters)) {
    f <- query$filters[[name]]
    if(!identical(f$kind,catalogue$fields[[name]]$kind)) tm_abort("Tipo de filtro inválido: ",name)
    if(!length(f$value) || anyNA(f$value)) tm_abort("Preencha os valores do filtro: ",catalogue$fields[[name]]$label)
    if(f$kind!="category") {
      bounds <- if(f$kind=="date") tryCatch(as.Date(f$value),error=function(e) NA) else suppressWarnings(as.numeric(f$value))
      if(length(bounds)!=2L || anyNA(bounds) || any(!is.finite(as.numeric(bounds))) || bounds[1]>bounds[2])
        tm_abort("Intervalo inválido: ",catalogue$fields[[name]]$label)
    }
  }
  invisible(query)
}

tm_where <- function(query,catalogue,include_filters=TRUE) {
  if(!length(query$periods) || !length(query$ufs)) tm_abort("Selecione período e abrangência.")
  columns <- catalogue$period_columns
  where <- if(query$system=="SIH-RD") paste0("(",paste(vapply(query$periods,function(period)
    paste0("(\"ANO_CMPT\" = ",as.integer(substr(period,1,4))," AND \"MES_CMPT\" = ",as.integer(substr(period,6,7)),")"),character(1)),collapse=" OR "),")") else
    paste0(tm_qi(columns)," IN (",paste(if(query$system=="CNES-ST") tm_ql(gsub("-","",query$periods,fixed=TRUE)) else as.integer(query$periods),collapse=","),")")
  if(length(query$ufs)<length(TM_UFS)) {
    geo <- catalogue$territories[[query$territory]]$expr
    if(is.null(geo)) tm_abort("Perspectiva territorial inválida.")
    where <- c(where,paste0(geo," IN (",paste(tm_ql(query$ufs),collapse=","),")"))
  }
  if(include_filters) for(name in names(query$filters)) {
    f <- query$filters[[name]]; expr <- catalogue$fields[[name]]$expr
    if(f$kind=="category") {
      clauses <- if(TM_MISSING %in% f$value) paste(expr,"IS NULL") else character()
      values <- setdiff(f$value,TM_MISSING)
      if(length(values)) clauses <- c(clauses,paste0("CAST(",expr," AS VARCHAR) IN (",paste(tm_ql(values),collapse=","),")"))
      where <- c(where,paste0("(",paste(clauses,collapse=" OR "),")"))
    } else if(f$kind=="date") {
      # Inclusive calendar dates, including timestamps throughout the final day.
      end <- as.character(as.Date(f$value[2])+1)
      where <- c(where,paste0(expr," >= CAST(",tm_ql(f$value[1])," AS DATETIME) AND ",expr," < CAST(",tm_ql(end)," AS DATETIME)"))
    } else where <- c(where,paste0(expr," BETWEEN ",as.numeric(f$value[1])," AND ",as.numeric(f$value[2])))
  }
  paste(where,collapse=" AND ")
}

tm_aggregate_sql <- function(query,catalogue,group,row,column) {
  stock <- tm_is_stock(query$system)
  groups <- unique(c(if(stock) "TM_PERIODO",group))
  select <- character(); by <- character()
  for(axis in c("periodo","linha","coluna")) {
    field <- switch(axis,periodo=if(stock) "TM_PERIODO" else "",linha=row,coluna=column)
    if(nzchar(field) && field %in% groups) {
      expr <- catalogue$fields[[field]]$expr
      select <- c(select,paste0(expr," AS ",axis))
      by <- c(by,axis)
      if(isTRUE(catalogue$fields[[field]]$municipality)) {
        select <- c(select,paste0(catalogue$fields[[field]]$name_expr," AS ",axis,"_nome"))
        by <- c(by,paste0(axis,"_nome"))
      }
    }
  }
  measure <- tm_measures(query$system); measure <- measure[measure$id==query$measure,,drop=FALSE]
  select <- c(select,"COUNT(*) AS registros")
  if(measure$op=="sum") select <- c(select,paste0("COALESCE(SUM(",tm_qi(measure$field),"), 0) AS valor"),paste0("COUNT(",tm_qi(measure$field),") AS validos"))
  if(measure$op=="distinct") {
    # SQL COUNT(DISTINCT) is approximate on Elasticsearch. Group IDs remotely
    # and count these exact groups, separately for each margin.
    select <- c(select,paste0(tm_qi(measure$field)," AS entidade"))
    by <- c(by,"entidade")
  }
  paste0("SELECT ",paste(select,collapse=", ")," FROM ",tm_qi(catalogue$index),
    " WHERE ",tm_where(query,catalogue),if(length(by)) paste0(" GROUP BY ",paste(by,collapse=", "))," LIMIT 2000001")
}

tm_run_query <- function(query,catalogue,config,progress=NULL) {
  tm_validate_query(query,catalogue)
  measure <- tm_measures(query$system); measure <- measure[measure$id==query$measure,,drop=FALSE]
  stock <- tm_is_stock(query$system)
  row <- if(stock && query$row=="TM_PERIODO") "" else query$row
  column <- if(stock && query$column=="TM_PERIODO") "" else query$column
  axes <- c(row,column); axes <- axes[nzchar(axes)]
  sets <- list(axes)
  if(length(axes)==2) sets <- c(sets,list(row,column,character()))
  else if(length(axes)==1) sets <- c(sets,list(character()))
  sqls <- character()
  results <- lapply(sets,function(group) {
    sql <- tm_aggregate_sql(query,catalogue,group,row,column); sqls <<- c(sqls,sql)
    x <- tm_sql(sql)
    if(nrow(x)>2000000) tm_abort("Consulta acima de dois milhões de grupos. Reduza os períodos ou aplique filtros.")
    if(!nrow(x)) return(NULL)
    for(name in intersect(c("valor","registros","validos"),names(x))) x[[name]] <- as.numeric(x[[name]])
    for(axis in c("periodo","linha","coluna")) if(!axis %in% names(x)) x[[axis]] <- if(axis=="periodo") "" else "Total"
    for(axis in c("periodo","linha","coluna")) {x[[axis]] <- as.character(x[[axis]]);x[[axis]][is.na(x[[axis]])] <- TM_MISSING}
    for(axis in c("linha","coluna")) {
      name <- paste0(axis,"_nome")
      if(!name %in% names(x)) x[[name]] <- NA_character_
      else x[[name]] <- as.character(x[[name]])
    }
    if(measure$op=="distinct") {
      dt <- data.table::as.data.table(x)
      x <- as.data.frame(dt[,list(valor=if(anyNA(entidade)) NA_real_ else as.numeric(.N),
        registros=sum(registros)),by=c("periodo","linha","coluna","linha_nome","coluna_nome")])
    } else if(measure$op=="count") x$valor <- x$registros
    else x$valor[x$validos<x$registros] <- NA_real_
    x$row_total <- !(nzchar(row) && row %in% group)
    x$col_total <- !(nzchar(column) && column %in% group)
    x[,c("periodo","linha","coluna","linha_nome","coluna_nome","valor","registros","row_total","col_total")]
  })
  long <- as.data.frame(data.table::rbindlist(results))
  if(!nrow(long)) long <- data.frame(periodo=character(),linha=character(),coluna=character(),valor=numeric(),
    linha_nome=character(),coluna_nome=character(),registros=numeric(),row_total=logical(),col_total=logical())
  row_field <- if(nzchar(query$row)) catalogue$fields[[query$row]] else NULL
  col_field <- if(nzchar(query$column)) catalogue$fields[[query$column]] else NULL
  for(axis in c("linha","coluna")) {
    municipal <- isTRUE((if(axis=="linha") row_field else col_field)$municipality)
    long[[axis]][long[[axis]]==TM_MISSING] <- if(municipal) NA_character_ else "Não informado"
  }
  long$linha[which(!long$row_total & long$linha=="Total")] <- "Total (categoria)"
  long$coluna[which(!long$col_total & long$coluna=="Total")] <- "Total (categoria)"
  long <- long[order(long$periodo,long$row_total,long$linha,long$col_total,long$coluna),,drop=FALSE]
  display <- long
  if(stock && query$column=="TM_PERIODO") {
    display$coluna <- display$periodo; display$col_total <- FALSE; display$periodo <- ""
    table <- tm_wide(display,FALSE,row_field,col_field)
  } else {
    table <- tm_wide(display,stock,row_field,col_field)
    if(stock && query$row=="TM_PERIODO" && "Linha" %in% names(table)) table$Linha <- NULL
  }
  warnings <- catalogue$warnings
  if(stock) warnings <- c(warnings,"Valores separados por período; não há total acumulado entre anos ou competências.")
  if(anyNA(long$valor)) warnings <- c(warnings,"Conteúdo ausente: células e totais afetados permanecem ausentes, sem imputação de zero.")
  if(!nrow(long) || all(long$registros==0)) warnings <- c(warnings,"Nenhum registro corresponde aos filtros aplicados.")
  list(table=table,long=long,query=query,measure=measure,warnings=warnings,
    provenance=data.frame(Fonte="PCDaS",Índice=catalogue$index,Perspectiva=catalogue$territories[[query$territory]]$label,
      Períodos=paste(query$periods,collapse=", "),check.names=FALSE),
    created_at=Sys.time(),catalogue_at=catalogue$checked_at,
    versions=list(rpcdas=config$rpcdas_sha,pipeline=config$pipeline),
    labels=list(row=if(nzchar(query$row)) catalogue$fields[[query$row]]$label else "Sem discriminação",
      column=if(nzchar(query$column)) catalogue$fields[[query$column]]$label else "Desativada",
      territory=catalogue$territories[[query$territory]]$label),sql=sqls)
}
