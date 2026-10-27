source("R/bootstrap.R");tm_load()
config <- tm_config();report <- list()
for(id in tm_available()$id) {
  t <- system.time(c <- tm_catalogue(id));cat(id,"catalogue",t[["elapsed"]],"s\n")
  period <- tail(c$periods,1);q <- tm_query(id,period,"AC",row="TM_UF_RES")
  t <- system.time(r <- tm_run_query(q,c,config))
  where <- switch(id,`SIM-DO`=paste0("ano_obito=",period," AND res_SIGLA_UF='AC'"),
    `SIM-DOFET`=paste0("ano_obito=",period," AND res_SIGLA_UF='AC'"),
    SINASC=paste0("ano_nasc=",period," AND res_SIGLA_UF='AC'"),
    `SIH-RD`=paste0("ANO_CMPT=",substr(period,1,4)," AND MES_CMPT=",as.integer(substr(period,6,7))," AND \"res_SIGLA_UF.keyword\"='AC'"),
    `CNES-ST`=paste0("COMPETEN='",gsub("-","",period,fixed=TRUE),"' AND \"uf_SIGLA_UF.keyword\"='AC'"),
    `POP-PCDAS`=paste0("ANO=",period," AND SIGLA_UF='AC'"))
  if(id=="CNES-ST") expected <- nrow(tm_sql(paste0('SELECT CNES FROM "datasus-cnes" WHERE ',where,' GROUP BY CNES')))
  else expected <- tm_sql(paste0("SELECT ",if(id=="POP-PCDAS") "SUM(POPULACAO)" else "COUNT(*)"," AS n FROM ",tm_qi(c$index)," WHERE ",where))$n
  stopifnot(tail(r$table$Total,1)==as.numeric(expected))
  report[[id]] <- list(period=period,ufs="AC",seconds=t[["elapsed"]],total=expected)
  if(id=="SIH-RD") {
    q$measure <- "value";s <- tm_run_query(q,c,config)
    expected <- tm_sql(paste0('SELECT SUM(VAL_TOT) AS n FROM "datasus-sih" WHERE ',where))$n
    stopifnot(isTRUE(all.equal(tail(s$table$Total,1),as.numeric(expected),tolerance=1e-6)))
  }
  if(tm_is_stock(id)) {
    q$periods <- tail(c$periods,2);q$column <- "TM_PERIODO"
    s <- tm_run_query(q,c,config)
    stopifnot(all(q$periods %in% names(s$table)),!"Total" %in% names(s$table))
  }
  cat(id,"state query",t[["elapsed"]],"s: OK\n")
}
c <- tm_catalogue("SIM-DO");q <- tm_query(c$id,"2022","AC",row="def_sexo",column="def_raca_cor")
q$filters <- list(def_sexo=list(kind="category",value="Feminino"),data_obito=list(kind="date",value=c("2022-01-01","2022-01-31")))
r <- tm_run_query(q,c,config)
x <- tm_sql('SELECT COUNT(*) AS n FROM "datasus-sim" WHERE ano_obito=2022 AND res_SIGLA_UF=\'AC\' AND def_sexo=\'Feminino\' AND data_obito>=CAST(\'2022-01-01\' AS DATETIME) AND data_obito<CAST(\'2022-02-01\' AS DATETIME)')
stopifnot(tail(r$table$Total,1)==x$n)
q$filters <- list(CAUSAMAT=list(kind="category",value=TM_MISSING));r <- tm_run_query(q,c,config)
x <- tm_sql('SELECT COUNT(*) AS n FROM "datasus-sim" WHERE ano_obito=2022 AND res_SIGLA_UF=\'AC\' AND CAUSAMAT IS NULL')
stopifnot(tail(r$table$Total,1)==x$n)
q$row <- q$column <- "";q$filters <- list(def_sexo=list(kind="category",value="' OR 1=1 --"))
stopifnot(tm_run_query(q,c,config)$table$Total==0)
q <- tm_query(c$id,tail(c$periods,1),"all",row="def_sexo")
t <- system.time(r <- tm_run_query(q,c,config));x <- tm_sql(paste0('SELECT COUNT(*) AS n FROM "datasus-sim" WHERE ano_obito=',q$periods))
stopifnot(tail(r$table$Total,1)==x$n)
report$national_sim <- list(period=q$periods,seconds=t[["elapsed"]],total=x$n)
q <- tm_query(c$id,"2022","AC")
stopifnot("120040" %in% tm_values(c,q,"TM_MUN_RES","rio")$values)
a <- tm_sql('SELECT def_sexo, COUNT(*) AS n FROM "datasus-sim" WHERE ano_obito=2022 AND res_SIGLA_UF=\'AC\' GROUP BY def_sexo',fetch_size=1)
stopifnot(sum(a$n)==4159)
path <- commandArgs(trailingOnly=TRUE)[1]
if(is.na(path)) path <- file.path(tempdir(),"pcdas-smoke.json")
jsonlite::write_json(list(validated_at=as.character(Sys.time()),cases=report),path,pretty=TRUE,auto_unbox=TRUE)
cat("All PCDaS checks passed. Report:",path,"\n")
