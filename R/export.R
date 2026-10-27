tm_metadata <- function(result) {
  query <- result$query
  values <- list(Base=query$system,Conteúdo=result$measure$label,Unidade=result$measure$unit,
    Linha=result$labels$row,Coluna=result$labels$column,Períodos=paste(query$periods,collapse=", "),
    UFs=paste(query$ufs,collapse=", "),
    Filtros=jsonlite::toJSON(query$filters,auto_unbox=TRUE),
    Gerado=as.character(result$created_at),Catálogo=as.character(result$catalogue_at),
    Fonte="PCDaS",Índice=result$provenance$Índice,Perspectiva=result$labels$territory,
    rpcdas=result$versions$rpcdas,
    Observações=paste(result$warnings,collapse="\n"))
  data.frame(Item=names(values),Valor=vapply(values,as.character,character(1)),check.names=FALSE)
}

tm_csv <- function(result,path) {
  # UTF-8 BOM makes accents readable when opening directly in Excel.
  con <- file(path,"wb"); on.exit(close(con),add=TRUE)
  writeBin(as.raw(c(0xef,0xbb,0xbf)),con)
  utils::write.table(result$table,con,sep=";",dec=",",row.names=FALSE,
                     col.names=TRUE,quote=TRUE,na="NA",qmethod="double")
  invisible(path)
}

tm_excel_ok <- function(result) nrow(result$table)<=1048575L && ncol(result$table)<=16384L &&
  nrow(result$provenance)<=1048575L

tm_xlsx <- function(result,path) {
  if (!tm_excel_ok(result)) tm_abort("O resultado excede os limites do Excel. Utilize CSV.")
  provenance <- result$provenance
  # Local server paths are operational details, not data provenance for users.
  provenance <- provenance[,setdiff(names(provenance),c("dbc_path","data_path")),drop=FALSE]
  for (name in names(provenance)) if (inherits(provenance[[name]],"POSIXt")) provenance[[name]] <- as.character(provenance[[name]])
  writexl::write_xlsx(list(Tabela=result$table,Metadados=tm_metadata(result),Fontes=provenance),path)
  invisible(path)
}
