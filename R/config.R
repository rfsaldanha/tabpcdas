`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

tm_config <- function(root=getwd()) {
  timeout <- suppressWarnings(as.numeric(Sys.getenv("TABPCDAS_TIMEOUT", "240")))
  if (!is.finite(timeout) || timeout<=0) stop("TABPCDAS_TIMEOUT deve ser positivo.")
  list(root=normalizePath(root,mustWork=TRUE),timeout=timeout,
       rpcdas_sha="a74a50a6c5396b79d8045927d7761e9f0be4b0b0",pipeline="4")
}
tm_init <- function(config) config
tm_abort <- function(...) stop(paste0(...),call.=FALSE)
tm_note <- function(progress,text) if(is.function(progress)) progress(text)
TM_UFS <- c(`11`="RO", `12`="AC", `13`="AM", `14`="RR", `15`="PA", `16`="AP",
            `17`="TO", `21`="MA", `22`="PI", `23`="CE", `24`="RN", `25`="PB",
            `26`="PE", `27`="AL", `28`="SE", `29`="BA", `31`="MG", `32`="ES",
            `33`="RJ", `35`="SP", `41`="PR", `42`="SC", `43`="RS", `50`="MS",
            `51`="MT", `52`="GO", `53`="DF")
TM_MISSING <- "__TM_MISSING__"
tm_is_stock <- function(id) id %in% c("CNES-ST","POP-PCDAS")
tm_qi <- function(x) paste0('"',gsub('"','""',x,fixed=TRUE),'"')
tm_ql <- function(x) paste0("'",gsub("'","''",as.character(x),fixed=TRUE),"'")
