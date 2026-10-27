tm_systems <- function() data.frame(
  id=c("SIM-DO","SIM-DOFET","SINASC","SIH-RD","CNES-ST","POP-PCDAS"),
  name=c("SIM — Mortalidade","SIM — Óbitos fetais","SINASC — Nascidos vivos",
         "SIH — Internações hospitalares","CNES — Estabelecimentos","População — Totais municipais"),
  index=c("datasus-sim","datasus-sim-dofet","datasus-sinasc","datasus-sih","datasus-cnes","datasus-pop"))

tm_available <- function() {
  if(!nzchar(Sys.getenv("pcdas_token"))) tm_abort("Credencial da PCDaS não configurada no servidor.")
  available <- tryCatch(rpcdas::list_pcdas_tables(),error=function(e)
    tm_abort("Não foi possível consultar os índices autorizados na PCDaS."))
  systems <- tm_systems()
  systems[systems$index %in% available,,drop=FALSE]
}

tm_measures <- function(id) {
  if(id=="POP-PCDAS") return(data.frame(id="population",label="População residente",op="sum",field="POPULACAO",unit="pessoas"))
  if(id=="CNES-ST") return(data.frame(id="establishments",label="Estabelecimentos distintos",op="distinct",field="CNES",unit="estabelecimentos"))
  x <- data.frame(id="count",label=switch(id,`SIH-RD`="AIHs (registros)",SINASC="Nascidos vivos","Óbitos"),
                  op="count",field=NA_character_,unit="registros")
  if(id=="SIH-RD") x <- rbind(x,data.frame(id=c("value","days"),label=c("Valor total das AIHs","Dias de permanência"),
    op="sum",field=c("VAL_TOT","DIAS_PERM"),unit=c("R$","dias")))
  x
}

tm_field_labels <- c(def_sexo="Sexo",def_raca_cor="Raça/cor",def_escol="Escolaridade",
  def_escol_mae="Escolaridade da mãe",def_est_civil="Estado civil",def_est_civil_mae="Estado civil da mãe",
  def_gestacao="Duração da gestação",def_gravidez="Tipo de gravidez",def_parto="Tipo de parto",
  def_loc_ocor="Local de ocorrência",def_loc_nasc="Local de nascimento",def_tipo_obito="Tipo de óbito",
  def_car_int="Caráter da internação",def_morte="Óbito na internação",def_nat_jur="Natureza jurídica",
  def_tp_unid="Tipo de estabelecimento",def_tp_prest="Tipo de prestador",def_esfera_a="Esfera administrativa",
  def_procedimento_realizado="Procedimento realizado",def_diag_princ_cap="Diagnóstico principal — capítulo CID",
  causabas_capitulo="Causa básica — capítulo CID",causabas_categoria="Causa básica — categoria CID",
  causabas_grupo="Causa básica — grupo CID",CAUSABAS="Causa básica — código CID",DIAG_PRINC="Diagnóstico principal — código CID",
  IDADEMAE="Idade da mãe",PESO="Peso ao nascer",idade_obito_anos="Idade no óbito (anos)",
  def_idade_anos="Idade (anos)",def_idade_18="Faixa etária",data_obito="Data do óbito",
  data_nasc="Data de nascimento",dt_inter="Data da internação",dt_saida="Data da saída",
  VAL_TOT="Valor total da AIH",DIAS_PERM="Dias de permanência")

tm_catalogue <- function(id,config=NULL) {
  systems <- tm_systems(); spec <- systems[systems$id==id,,drop=FALSE]
  if(nrow(spec)!=1L) tm_abort("Base inválida.")
  schema <- tm_sql(paste("DESCRIBE",tm_qi(spec$index)))
  if(!all(c("column","type","mapping") %in% names(schema))) tm_abort("Esquema da PCDaS indisponível.")
  resolve <- function(field) {
    if(paste0(field,".keyword") %in% schema$column) field <- paste0(field,".keyword")
    if(!field %in% schema$column) tm_abort("Campo necessário ausente na PCDaS: ",field)
    tm_qi(field)
  }
  # Only aggregateable fields; identifiers, free text and coordinates are excluded.
  usable <- schema[schema$mapping %in% c("keyword","long","integer","float","double","datetime","boolean") &
    !grepl("CPF|CNPJ|CEP|CONTADOR|contador|^NUM|^N_AIH$|^ATEST|^LINHA|^CRM$|^CODCART|^CODINST|^NOME|^NM_|^END|^CODBAI|^watermark|LATITUDE|LONGITUDE|ALTITUDE|AREA$|^CNES$",schema$column),,drop=FALSE]
  fields <- setNames(lapply(seq_len(nrow(usable)),function(i) {
    name <- usable$column[i]; base <- sub("\\.keyword$","",name)
    label <- unname(tm_field_labels[base])
    if(is.na(label)) label <- gsub("_"," ",base,fixed=TRUE)
    kind <- if(usable$type[i]=="TIMESTAMP") "date" else if(usable$type[i] %in% c("REAL","DOUBLE","INTEGER","BIGINT")) "number" else "category"
    list(field=name,label=paste0(label," [",base,"]"),kind=kind,expr=tm_qi(name))
  }),usable$column)
  # Prioritize decoded categorical fields over their raw representations.
  fields <- fields[order(!startsWith(names(fields),"def_"),vapply(fields,`[[`,character(1),"label"))]
  period <- switch(id,`SIM-DO`=resolve("ano_obito"),`SIM-DOFET`=resolve("ano_obito"),
    SINASC=resolve("ano_nasc"),`SIH-RD`=paste0("CONCAT(CONCAT(CAST(",resolve("ANO_CMPT")," AS VARCHAR), '-'), RIGHT(CONCAT('0', CAST(",resolve("MES_CMPT")," AS VARCHAR)), 2))"),
    `CNES-ST`=paste0("CONCAT(CONCAT(LEFT(",resolve("COMPETEN"),", 4), '-'), RIGHT(",resolve("COMPETEN"),", 2))"),
    `POP-PCDAS`=resolve("ANO"))
  period <- paste0("CAST(",period," AS VARCHAR)")
  territories <- if(id %in% c("SIM-DO","SIM-DOFET","SINASC","SIH-RD")) {
    occurrence <- switch(id,SINASC="nasc",`SIH-RD`="int","ocor")
    list(residence=list(label="Residência",expr=resolve("res_SIGLA_UF")),
         occurrence=list(label=switch(id,SINASC="Nascimento",`SIH-RD`="Atendimento","Ocorrência"),expr=resolve(paste0(occurrence,"_SIGLA_UF"))))
  } else list(location=list(label=if(id=="CNES-ST") "Localização do estabelecimento" else "Território de referência",
    expr=resolve(if(id=="CNES-ST") "uf_SIGLA_UF" else "SIGLA_UF")))
  primary <- territories[[1]]$expr
  prefix <- if(id=="CNES-ST") "mun_" else if(id=="POP-PCDAS") "" else "res_"
  code <- resolve(paste0(prefix,"codigo_adotado")); name <- resolve(paste0(prefix,"MUNNOME"))
  municipality <- function(field,label,code,name) list(field=field,label=label,kind="category",
    expr=paste0("CAST(",code," AS VARCHAR)"),municipality=TRUE,name_expr=name)
  derived <- list(
    TM_PERIODO=list(field="TM_PERIODO",label=if(id %in% c("SIH-RD","CNES-ST")) "Competência" else "Ano de referência",kind="category",expr=period),
    TM_UF_RES=list(field="TM_UF_RES",label=if(length(territories)==2) "UF de residência" else "UF",kind="category",expr=primary),
    TM_MUN_RES=municipality("TM_MUN_RES",if(length(territories)==2) "Município de residência" else "Município",code,name))
  if(length(territories)==2) derived$TM_MUN_OCOR <- municipality("TM_MUN_OCOR",
    switch(id,SINASC="Município de nascimento",`SIH-RD`="Município de atendimento","Município de ocorrência"),
    resolve(paste0(occurrence,"_codigo_adotado")),resolve(paste0(occurrence,"_MUNNOME")))
  # Source reference fields also carry their matching code/name pair, so
  # selecting a municipal reference through its technical name stays unambiguous.
  for(municipal in derived[vapply(derived,function(f)isTRUE(f$municipality),logical(1))]) {
    for(raw in names(fields)) if(fields[[raw]]$expr %in% c(municipal$name_expr,
        sub("^CAST\\((.*) AS VARCHAR\\)$","\\1",municipal$expr))) {
      fields[[raw]]$expr <- municipal$expr
      fields[[raw]]$name_expr <- municipal$name_expr
      fields[[raw]]$municipality <- TRUE
      fields[[raw]]$kind <- "category"
      fields[[raw]]$label <- paste0(municipal$label," [",raw,"]")
    }
  }
  fields <- c(derived,fields)
  period_columns <- switch(id,`SIM-DO`="ano_obito",`SIM-DOFET`="ano_obito",SINASC="ano_nasc",
    `SIH-RD`=c("ANO_CMPT","MES_CMPT"),`CNES-ST`="COMPETEN",`POP-PCDAS`="ANO")
  columns_sql <- paste(tm_qi(period_columns),collapse=", ")
  raw_periods <- tm_sql(paste0("SELECT ",columns_sql," FROM ",tm_qi(spec$index),
    " GROUP BY ",columns_sql," ORDER BY ",columns_sql))
  periods <- if(!nrow(raw_periods)) character() else if(id=="SIH-RD")
    sprintf("%04d-%02d",as.integer(raw_periods$ANO_CMPT),as.integer(raw_periods$MES_CMPT)) else if(id=="CNES-ST")
      paste0(substr(raw_periods$COMPETEN,1,4),"-",substr(raw_periods$COMPETEN,5,6)) else as.character(raw_periods[[1]])
  pattern <- if(id %in% c("SIH-RD","CNES-ST")) "^[0-9]{4}-(0[1-9]|1[0-2])$" else "^[0-9]{4}$"
  periods <- sort(unique(periods[!is.na(periods) & grepl(pattern,periods)]))
  if(!length(periods)) tm_abort("Nenhum período válido disponível nesta base.")
  measures <- tm_measures(id)
  if(any(!na.omit(measures$field) %in% schema$column)) tm_abort("Conteúdo indisponível no esquema da PCDaS.")
  list(id=id,index=spec$index,fields=fields,periods=periods,period_columns=period_columns,territories=territories,
       checked_at=Sys.time(),warnings=if(id=="POP-PCDAS") "O índice populacional contém totais municipais por ano, sem sexo ou faixa etária." else character())
}
