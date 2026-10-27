# Explicit developer command: records small, real, aggregate-only responses.
source("R/bootstrap.R");tm_load()
c <- tm_catalogue("SIM-DO");q <- tm_query(c$id,"2022","AC","def_sexo","def_raca_cor")
original <- tm_sql;recorded <- list()
tm_sql <- function(sql,fetch_size=5000L){x<-original(sql,fetch_size);recorded[[sql]]<<-x;x}
r <- tm_run_query(q,c,tm_config())
qrow <- q;qrow$column <- "";tm_run_query(qrow,c,tm_config())
saveRDS(list(catalogue=c,query=q,responses=recorded,source="PCDaS datasus-sim, AC, 2022",retrieved=Sys.time()),"tests/fixtures/sim-aggregates.rds")
token <- Sys.getenv("pcdas_token")
sql <- 'SELECT def_sexo, CAUSAMAT, COUNT(*) AS n FROM "datasus-sim" WHERE ano_obito=2022 AND res_SIGLA_UF=\'AC\' GROUP BY def_sexo, CAUSAMAT'
pages <- getFromNamespace("pcdas_query_request","rpcdas")(body=list(token=list(token=token),sql=list(sql=list(query=sql,fetch_size=1))),pcdas_token=token)
# Responses contain no token, cursor or record-level data.
pages <- lapply(pages,function(page){page$cursor <- NULL;page})
saveRDS(list(sql=sql,pages=pages,retrieved=Sys.time()),"tests/fixtures/paginated-null.rds")
