tm_load <- function(root = getwd(), envir = parent.frame()) {
  for (name in c("config", "catalogue", "data", "tabulate", "presentation", "export", "queue", "filters", "ui"))
    sys.source(file.path(root,"R",paste0(name,".R")), envir=envir)
  invisible(TRUE)
}
