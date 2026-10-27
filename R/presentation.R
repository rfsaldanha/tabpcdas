tm_axis_columns <- function(data,axis,field,both=FALSE) {
  municipal <- isTRUE(field$municipality)
  total <- data[[if(axis=="linha") "row_total" else "col_total"]]
  if(!municipal) return(stats::setNames(data.frame(data[[axis]],check.names=FALSE),if(axis=="linha") "Linha" else "Coluna"))
  code <- as.character(data[[axis]]); code[total] <- ""
  name <- data[[paste0(axis,"_nome")]]
  name[is.na(name) | !nzchar(name)] <- "Não informado"
  name[total] <- "Total"
  label <- if(both) sub(" \\[.*$","",field$label) else "Município"
  out <- data.frame(code,name,check.names=FALSE)
  names(out) <- c(paste("Código do",sub("^Município","município",label)),label)
  out
}

tm_long_table <- function(long,stock,row_field,col_field) {
  both <- isTRUE(row_field$municipality) && isTRUE(col_field$municipality)
  out <- if(stock) data.frame(Período=long$periodo,check.names=FALSE) else data.frame(row.names=seq_len(nrow(long)))
  row <- tm_axis_columns(long,"linha",row_field,both)
  col <- tm_axis_columns(long,"coluna",col_field,both)
  if(both && identical(names(row),names(col))) {
    names(row) <- paste(names(row),"(linha)");names(col) <- paste(names(col),"(coluna)")
  }
  cbind(out,row,col,data.frame(Valor=long$valor,check.names=FALSE))
}

tm_wide <- function(long,stock,row_field=NULL,col_field=NULL) {
  if(!nrow(long)) return(data.frame(Mensagem="Nenhum registro para os filtros aplicados."))
  cols <- unique(long$coluna[order(long$col_total,long$coluna)])
  key_fields <- c("periodo","linha",if(isTRUE(row_field$municipality)) "linha_nome","row_total")
  keys <- unique(long[,key_fields,drop=FALSE])
  # Municipal columns are always long: CSV and Excel share the same rectangular
  # structure, with explicit code/name columns and no compound headers.
  if(isTRUE(col_field$municipality) || length(cols)>500 || length(cols)*nrow(keys)>2000000)
    return(tm_long_table(long,stock,row_field,col_field))
  out <- tm_axis_columns(keys,"linha",row_field)
  if(stock) out <- cbind(data.frame(Período=keys$periodo,check.names=FALSE),out)
  colnames <- make.unique(c(names(out),cols),sep=" · ")[-seq_len(ncol(out))]
  key_table <- data.table::as.data.table(keys)
  for(i in seq_along(cols)) {
    selected <- data.table::as.data.table(long[long$coluna==cols[[i]],,drop=FALSE])
    # Join on separate fields, including the name. Homonyms and missing values
    # never depend on parsing a separator or comparing a display label.
    index <- selected[key_table,on=key_fields,which=TRUE]
    values <- selected$valor[index]; values[is.na(index)] <- 0
    out[[colnames[[i]]]] <- values
  }
  out
}
