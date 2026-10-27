# All data access goes through the pinned rpcdas API. No local data cache.
tm_decode_pages <- function(pages) {
  if(!length(pages)) return(data.frame())
  columns <- pages[[1]]$columns
  if(!length(columns)) tm_abort("Resposta da PCDaS sem esquema.")
  names <- vapply(columns,`[[`,character(1),"name")
  types <- vapply(columns,`[[`,character(1),"type")
  parts <- lapply(pages,function(page) {
    if(length(page$columns) && !identical(vapply(page$columns,`[[`,character(1),"name"),names)) tm_abort("Esquema alterado durante a paginação da PCDaS.")
    rows <- page$rows
    if(any(lengths(rows)!=length(names))) tm_abort("Resposta incompleta da PCDaS.")
    result <- lapply(seq_along(names),function(i) {
      values <- vapply(rows,function(row) if(is.null(row[[i]])) NA_character_ else as.character(row[[i]]),character(1))
      if(types[i] %in% c("byte","short","integer","long","float","half_float","double","scaled_float","unsigned_long")) {
        parsed <- suppressWarnings(as.numeric(values))
        if(any(!is.na(values) & is.na(parsed))) tm_abort("Valor numérico inválido na resposta da PCDaS.")
        parsed
      } else values
    })
    as.data.frame(stats::setNames(result,names),stringsAsFactors=FALSE)
  })
  as.data.frame(data.table::rbindlist(parts))
}

tm_sql <- function(sql,fetch_size=65000L) {
  token <- Sys.getenv("pcdas_token")
  if(!nzchar(token)) tm_abort("Credencial da PCDaS não configurada no servidor.")
  # The public generic wrapper drops NULL cells with unlist(). Use its same
  # paginated request helper, pinned and signature-tested, with a typed decoder.
  request <- getFromNamespace("pcdas_query_request","rpcdas")
  body <- list(token=list(token=token),sql=list(sql=list(query=sql,fetch_size=fetch_size)))
  pages <- tryCatch(request(body=body,pcdas_token=token,max_tries=3),
    error=function(e) tm_abort("Não foi possível concluir a consulta à PCDaS. Tente novamente ou reduza o recorte."))
  tm_decode_pages(pages)
}

tm_values <- function(catalogue,query,field,search="") {
  if(!field %in% names(catalogue$fields)) tm_abort("Variável de filtro inválida.")
  f <- catalogue$fields[[field]];expr <- f$expr
  municipal <- isTRUE(f$municipality)
  where <- tm_where(query,catalogue,include_filters=FALSE)
  if(nzchar(trimws(search))) {
    expressions <- c(expr,if(municipal) f$name_expr)
    clauses <- vapply(expressions,function(x) paste0("LOCATE(",tm_ql(tolower(trimws(search))),
      ", LCASE(CAST(",x," AS VARCHAR))) > 0"),character(1))
    where <- paste0(where," AND (",paste(clauses,collapse=" OR "),")")
  }
  sql <- paste0("SELECT ",expr," AS value",if(municipal) paste0(", ",f$name_expr," AS name"),
    ", COUNT(*) AS n FROM ",tm_qi(catalogue$index)," WHERE ",where," AND ",expr,
    " IS NOT NULL GROUP BY value",if(municipal) ", name", " ORDER BY ",
    if(municipal) "name, value" else "value"," LIMIT 201")
  x <- tm_sql(sql);more <- nrow(x)>200;x <- head(x,200)
  options <- data.frame(value=character(),label=character())
  if(nrow(x)) {
    values <- unique(as.character(x$value))
    labels <- vapply(values,function(value) {
      if(!municipal) return(value)
      names <- unique(x$name[as.character(x$value)==value]);names <- names[!is.na(names) & nzchar(names)]
      paste0(if(length(names)) paste(names,collapse=" / ") else "Não informado"," — ",value)
    },character(1))
    options <- data.frame(value=values,label=unname(labels),stringsAsFactors=FALSE)
  }
  list(values=options$value,options=options,more=more)
}
