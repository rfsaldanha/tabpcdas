# Small, real aggregate responses. No credentials, cursors or microdata are saved.
source("R/bootstrap.R");tm_load()
original <- tm_sql;responses <- list()
c <- tm_catalogue("SIM-DO")
tm_sql <- function(sql,...) {x<-original(sql,...);responses[[sql]]<<-x;x}
queries <- list(row=tm_query(c$id,"2022","AC","TM_MUN_RES","def_sexo"),
 col=tm_query(c$id,"2022","AC","def_sexo","TM_MUN_RES"),
 both=tm_query(c$id,"2022","AC","TM_MUN_RES","TM_MUN_OCOR"),
 homonyms=tm_query(c$id,"2022","all","TM_MUN_RES",filters=list(res_MUNNOMEX=list(kind="category",value="BOM JESUS"))))
invisible(lapply(queries,function(q)tm_run_query(q,c,tm_config())))
saveRDS(list(catalogue=c,queries=queries,responses=responses,
  source="PCDaS datasus-sim, 2022; AC and Brazil/BOM JESUS",retrieved=Sys.time()),"tests/fixtures/municipalities.rds")
stocks <- list()
for(id in c("POP-PCDAS","CNES-ST")) {
 c <- tm_catalogue(id);responses <- list()
 q <- tm_query(id,tail(c$periods,2),"AC","TM_MUN_RES","TM_PERIODO")
 result <- tm_run_query(q,c,tm_config())
 qcol <- q;qcol$row <- "TM_PERIODO";qcol$column <- "TM_MUN_RES"
 resultcol <- tm_run_query(qcol,c,tm_config())
 stocks[[id]] <- list(catalogue=c,query=q,column_query=qcol,responses=responses)
}
saveRDS(list(cases=stocks,source="PCDaS population and CNES, AC, latest two periods",retrieved=Sys.time()),"tests/fixtures/municipal-stocks.rds")
