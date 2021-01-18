
# Header
# Filename:      io.R
# Description:   This library, is a part of package viser and is used for data import and export to/from other environments
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    28 October 2016
# Last Revision: 17 July 2019
# Version:       1.0.1

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.0     28 October 2016    Initial Issue: Started with function readODBC().
# 0.1.0     21 November 2016   Added filters to function readODBC().
# 0.2.0     01 December 2016   Function sqlFilter() generates script for filtering. Functions sqlFilterComponent.*() removed.
# 0.2.1     01 December 2016   Function sqlScript() generates complete sql query.
# 0.2.2     01 December 2016   Function readODBC() calls sqlScript() first to generate the query and then executes the query.
# 0.2.3     24 January 2017    Function readODBC() transfers additional arguments passed to function sqlQuery() in package RODBC.
# 0.3.0     17 May 2017        Function rScriptFilter() added, converts a filter into a R script of a condition
# 0.3.1     01 August 2017     Function rScriptFilter() exported.
# 0.3.2     15 May 2018        Function sqlScript() modified: changes column names with 'AS' if argument 'fields' is a named vector
# 0.3.3     12 September 2018  Function runSQL() added
# 1.0.0     26 June 2019       spark and athena tools and AGGERGATOR module added.
# 1.0.1     17 July 2019       Function sqlFilter() modified: small bug fixed.


################## ODBC TOOLS: ###############################

# This function reads a table by ODBC and generates an object of type WIDETABLE
# Reads a table using ODBC
# similar functions will be like this:
# readODBC.WIDETABLE
# readODBC.nibeTree
#
# Examples:
#
# D = readODBC('ASET_VALU_CURR', fields = c('ASET_I', 'ASET_VALU_D', 'ASET_VALU_A'), dbName = 'PVDATA')

# script.sql.select = function(fields = '*'){}
# script.sql.from   = function(dbName,tableName){}
# script.sql.where  =

valid.filter.elements = c('domain', 'min', 'max', 'type', 'na.rm', 'equal', 'query')
valid.field.types     = c('nominal', 'numeric', 'date', 'time')

#' @export
sqlFilter = function(colName, filter, vf = T){
  if(vf){
    verify(filter, 'list', names_domain = valid.filter.elements, varname = 'filter')
    filter$type  <- verify(filter$type, 'character', domain = valid.field.types, varname = 'filter$type', default = 'nominal')
    filter$na.rm <- verify(filter$na.rm, 'logical', domain = c(T,F), varname = 'filter$na.rm', default = F)
    filter$equal <- verify(filter$equal, 'logical', domain = c(T,F), varname = 'filter$equal', default = F)
    filter$query <- verify(filter$query, 'character', varname = 'filter$query')
  }
  if(!is.null(filter$query)){
    return(paste0(colName, " IN (", filter$query, ")"))
  }
  if (filter$equal){
    minopr = " >= "
    maxopr = " <= "
  } else {
    minopr = " > "
    maxopr = " < "
  }
  scr   = ""
  joint = ""
  switch(filter$type,
         'nominal' = {
           if(!is.null(filter$domain)){
             if (!inherits(filter$domain, 'character')){filter$domain %<>% as.character}
             if(length(filter$domain) == 1){
               scr = paste0(colName, " = '",  filter$domain, "'")
             } else {
               scr = paste0(colName, " IN ('",  paste(filter$domain, collapse = "','"), "')")
             }
             joint = " AND "
           }
           if(filter$na.rm){
             scr = paste0(scr, joint, colName, " IS NOT NULL")
           }
         },
         'numeric' = {
           if(!is.null(filter$domain)){
             if (!inherits(filter$domain, 'numeric')){filter$domain %<>% as.numeric}
             if(length(filter$domain) == 1){
               scr = paste0(colName, " = ",  filter$domain)
             } else {
               scr = paste0(colName, " IN (",  paste(filter$domain, collapse = ", "), ")")
             }
             joint = " AND "
           }
           if (!is.null(filter$min)){
             scr = paste0(colName, minopr, filter$min)
             joint = " AND "
           }
           if (!is.null(filter$max)){
             scr = paste0(scr, joint, colName, maxopr, filter$max)
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, colName, " IS NOT NULL")}
         },
         'date'    = {
           if (!is.null(filter$min)){
             if(vf){verify(filter$min, valid.time.classes, varname = 'filter$min')}
             if (inherits(filter$min, 'character')){minDate = char2Date(filter$min)} else {minDate = as.Date(filter$min)}
             minDate = paste0("CAST(", "'", as.character(minDate), "'", " AS DATE)")
             scr = paste0(colName, minopr, minDate)
             joint = " AND "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxDate = char2Date(filter$max)} else {maxDate = as.POSIXlt(filter$max)}
             maxDate = paste0("CAST(", "'", as.character(maxDate), "'", " AS DATE)")
             scr = paste0(scr, joint, colName, maxopr, maxDate)
             
             # maxDate = as.character(maxDate)
             # scr = paste0(scr, joint, colName, maxopr, "'", maxDate, "'")
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, colName, " IS NOT NULL")}
         },
         'time'    = {
           if (!is.null(filter$min)){
             verify(filter$min, valid.time.classes, varname = 'filter$min')
             # if (inherits(filter$min, 'character')){minTime = char2Time(filter$min)} else {minTime = as.POSIXlt(filter$min)}
             minTime = as.time(filter$min, target_class = 'POSIXlt')
             minTime = as.character(minTime, format = '%Y-%m-%d %H:%M:%S')
             minTime = paste0('CAST(', "'", minTime, "'", " AS TIMESTAMP)")
             scr = paste0(colName, minopr, minTime)
             joint = " AND "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxTime = char2Time(filter$max)} else {maxTime = as.POSIXlt(filter$max)}
             maxTime = as.character(maxTime, format = '%Y-%m-%d %H:%M:%S')
             maxTime = paste0('CAST(', "'", maxTime, "'", " AS TIMESTAMP)")
             scr = paste0(scr, joint, colName, maxopr, maxTime)
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, colName, " IS NOT NULL")}
         })
  
  return(scr)
}

#' @export
rScriptFilter = function(colName, filter, vf = T){
  if(vf){
    verify(filter, 'list', names_domain = valid.filter.elements, varname = 'filter')
    filter$type  <- verify(filter$type, 'character', domain = valid.field.types, varname = 'filter$type', default = 'nominal')
    filter$na.rm <- verify(filter$na.rm, 'logical', domain = c(T,F), varname = 'filter$na.rm', default = F)
    filter$equal <- verify(filter$equal, 'logical', domain = c(T,F), varname = 'filter$equal', default = F)
    filter$query <- verify(filter$query, 'character', varname = 'filter$query')
  }
  
  if (filter$equal){
    minopr = " >= "
    maxopr = " <= "
  } else {
    minopr = " > "
    maxopr = " < "
  }
  scr   = ""
  joint = ""
  switch(filter$type,
         'nominal' = {
           if(!is.null(filter$domain)){
             if (!inherits(filter$domain, 'character')){filter$domain %<>% as.character}
             scr = paste0(colName, " %in% c('",  paste(filter$domain, collapse = "','"), "')")
             joint = " & "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         },
         'numeric' = {
           if (!is.null(filter$min)){
             scr = paste0(colName, minopr, filter$min)
             joint = " & "
           }
           if (!is.null(filter$max)){
             scr = paste0(scr, joint, colName, maxopr, filter$max)
             joint = " & "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         },
         'date'    = {
           if (!is.null(filter$min)){
             if(vf){verify(filter$min, valid.time.classes, varname = 'filter$min')}
             if (inherits(filter$min, 'character')){minDate = char2Date(filter$min)} else {minDate = as.Date(filter$min)}
             minDate = as.character(minDate)
             scr = paste0(colName, minopr, "'", minDate, "'")
             joint = " & "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxDate = char2Date(filter$max)} else {maxDate = as.POSIXlt(filter$max)}
             maxDate = as.character(maxDate)
             scr = paste0(scr, joint, colName, maxopr, "('", maxDate, "' %>% as.Date)")
             joint = " & "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         },
         'time'    = {
           if (!is.null(filter$min)){
             verify(filter$min, valid.time.classes, varname = 'filter$min')
             # if (inherits(filter$min, 'character')){minTime = char2Time(filter$min)} else {minTime = as.POSIXlt(filter$min)}
             minTime = as.time(filter$min, target_class = 'POSIXlt')
             minTime = as.character(minTime, format = '%Y-%m-%d %H:%M:%S')
             scr = paste0(colName, minopr, "('", minTime, "' %>% as.POSIXlt)")
             joint = " & "
           }
           
           if (!is.null(filter$max)){
             verify(filter$max, valid.time.classes, varname = 'filter$max')
             if (inherits(filter$max, 'character')){maxTime = char2Time(filter$max)} else {maxTime = as.POSIXlt(filter$max)}
             maxTime = as.character(maxTime, format = '%Y-%m-%d %H:%M:%S')
             scr = paste0(scr, joint, colName, maxopr,"('", maxTime, "' %>% as.POSIXlt)")
             joint = " AND "
           }
           if(filter$na.rm){scr = paste0(scr, joint, "!is.na(", colName, ")")}
         })
  
  return(scr)
}


#' @export
sqlScript = function(tableName, fields = NULL, dbName = 'UDRBSCMS', filter = NULL, vf = T){
  if(vf){
    tableName  = verify(tableName, 'character', lengths = 1, varname = 'tableName')
    fields     = verify(fields, 'character', varname = 'fields', default = '*')
    filter     = verify(filter, 'list', varname = 'filter')
  }
  
  fldnms = names(fields)
  if(is.null(fldnms)){fields.str = paste(fields, collapse = " , ")} else {
    assert(length(fldnms) == length(fields))
    fldnms[fldnms == ''] <- fields[fldnms == '']
    fields.str = paste(paste(fields, 'AS', fldnms), collapse = " , ")
  }
  
  query      = paste0("SELECT ", fields.str, " from ", dbName, ".", tableName)
  
  if (!is.null(filter)){
    first = T
    query = query %++% " WHERE "
    for (fn in names(filter)){
      if (!first){query = query %++% " AND "}
      qfn   = sqlFilter(fn, filter[[fn]], vf = vf)
      query = query %++% qfn
      if (qfn != ""){first = F}
    }
  }
  return(query)
}


#' @export
readODBC <- function(tableName, fields = NULL, dbName = 'UDRBSCMS', filter = NULL, dsn = 'Teradata_Prod', vf = T){
  # Verifications:
  if(vf){assert(require(RODBC), "Package 'RODBC' is not installed!", err_src = match.call()[[1]])}
  
  channel    = odbcConnect(dsn = dsn, ...)
  D          = sqlQuery(channel = channel, query = sqlScript(tableName = tableName, fields = fields, dbName = dbName, filter = filter, vf = vf), ...)
  close(channel)
  return(D)
}

#' @export
runSQL <- function(query, ...){
  channel    = odbcConnect(...)
  D          = sqlQuery(channel = channel, query = query)
  close(channel)
  return(D)
}

################## SPARK TOOLS ###############################

spark.local = function(mem.exec = "64G", mem.shell_driver = "64G", mem.shell.exec = "64G", mem.max_result = "4G", mem.yarn_exec_overhead = "16G"){  
  config <- spark_config()
  config$spark.executor.memory <- mem.exec
  config[['sparklyr.shell.driver-memory']] <- mem.shell_driver
  config[['sparklyr.shell.executor-memory']] <- mem.shell.exec
  config[['spark.driver.maxResultSize']] <- mem.max_result
  config$spark.yarn.executor.memoryOverhead <- mem.yarn_exec_overhead
  spark_connect(master = "local", config = config)
}


#' @export
spark.read_s3 = function(path){
  sc <- sparklyr::spark_connect(master = 'local')
  el <- sparklyr::spark_read_csv(sc, name = 'el', path = path)
  return(el)
}


# bins numeric columns to categorical 
# binners is a list containing:
# input_col = colname to be binned, must have numeric values
# output_col = colname to be generated as a result of binning
# splits = named numeric vector containing splits and labels for them
#' @export
spark.bin = function(df, binners){
  if(inherits(df, c('data.frame', 'tibble'))){
    for (b in binners){
      df %>% pull(b$input_col) %>% cut(breaks = b$splits %>% unname, labels = names(b$splits)) -> df[, b$output_col]
    }
  } else if (inherits(df, 'tbl_spark')){
    
    df %<>% sparklyr::ft_bucketizer(
      input_cols   = binners %>% lapply(function(x) x$input_col) %>% unlist %>% as.character,
      output_cols  = binners %>% lapply(function(x) x$output_col) %>% unlist %>% as.character,
      splits_array = binners %>% lapply(function(x) x$splits %>% unname))
    
    for(b in binners){
      df %<>% rename(indexed = b$output_col) %>% sparklyr::ft_index_to_string(input_col = 'indexed', output_col = b$output_col, labels = b$splits %>% names) %>% 
        select(- indexed)
    }
    
  } else stop("Unsupported class for argument 'df'")
  return(df)
}

# 2btransferred to gener or niraspark (sparker)?
#' @export
spark.rename = function(tbl, ...){
  ns = c(...) %>% verify('character')
  lb = names(ns)
  if(is.null(lb)){return(tbl)}
  scr = paste0("tbl %>% dplyr::rename(", lb %>% paste(ns, sep = ' = ') %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' @export
spark.mutate = function(tbl, ...){
  ns = c(...) %>% verify('character')
  lb = names(ns)
  if(is.null(lb)){return(tbl)}
  scr = paste0("tbl %>% dplyr::mutate(", lb %>% paste(ns, sep = ' = ') %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' @export
spark.select = function(tbl, ...){
  ns = c(...) %>% verify('character') %>% intersect(colnames(tbl))
  if(length(ns) == 0){return(tbl[c()])}
  scr = paste0("tbl %>% dplyr::select(", ns %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' @export
spark.unselect = function(tbl, ...){
  ns = c(...) %>% verify('character') %>% intersect(colnames(tbl))
  if(length(ns) == 0){return(tbl)}
  scr = paste0("tbl %>% dplyr::select(", paste('-',ns) %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' @export
spark.dummify = function(tbl, ..., keep_original = T, keep_index = F){
  cat_cols = c(...) %>% verify('character', domain = colnames(tbl), default = character()) %>% unique
  if(length(cat_cols) == 0) {return(tbl)}
  
  # scr     <- paste0('tbl %>% filter(', paste(paste(cat_cols, '>= 0'), collapse = ' ,'), ')')
  scr     <- paste0('tbl %>% mutate(', paste0(paste0(cat_cols, ' = as.character(abs(as.integer(', cat_cols, ')))'), collapse = ' ,'), ')')
  tbl     <- parse(text = scr) %>% eval
  uniques <- list()
  for(col in cat_cols){
    icn <- col %>% paste('Index', sep = '_') # icn: indexed column name
    tbl %<>% ft_string_indexer(input_col = col, output_col = icn, string_order_type = 'alphabetDesc')
    
    scr  <- paste0("tbl %>% distinct(", col, ", ", icn, ") %>% collect")
    parse(text = scr) %>% eval -> uniques[[col]]
    if(nrow(uniques[[col]]) > 2){
      labels <- paste(col, uniques[[col]] %>% pull(col), sep= '_')
      # uniques[[col]] <- uniques[[col]][- length(uniques[[col]])]
      tbl %<>% ft_one_hot_encoder(icn, 'outlist', drop_last = F) %>% 
        sdf_separate_column('outlist', into = labels) %>% select(-outlist)
      if(!keep_original){
        tbl <- parse(text = paste0("tbl %>% select(-", col, ")")) %>% eval
      }
      if(!keep_index){
        tbl <- parse(text = paste0("tbl %>% select(-", icn, ")")) %>% eval
      }
    }
  }
  return(tbl) 
}

################## ATHENA SQL TOOLS ###############################

#' @export
athena.buildTable = function(conn, dsName, tblName, column_types, id_columns, s3_data_path, drop_if_exists = T, format = c('parquet', 'csv')){
  cursor = conn$cursor()
  format = match.arg(format)
  if(drop_if_exists){
    # Delete existing table
    cursor$execute("DROP TABLE if exists " %>% paste0(dsName, '.', tblName))
  }
  
  column_types = c(caseid = 'string', eventType = 'STRING', eventTime = 'TIMESTAMP', variable = 'STRING', value  = 'FLOAT')
  column_types = toupper(column_types)
  column_types %>% names %>% tolower -> names(column_types)
  
  structure = paste0('(', column_types %>% names %>% paste(column_types) %>% paste(collapse = ', '), ')')
  
  column_types[id_columns %>% tolower] -> id_cols
  partition    = paste0('PARTITIONED BY (', id_cols %>% names %>% paste(id_cols) %>% paste(collapse = ', '), ')')
  whatsthis    = 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
  if(format == 'parquet'){
    qry        = paste0("CREATE EXTERNAL TABLE ", dsName, ".", tblName, structure, " STORED AS PARQUET LOCATION '", s3_data_path, "'", " tblproperties ('parquet.compress'='SNAPPY');")
  } else {
    qry        = paste0("CREATE EXTERNAL TABLE ", dsName, ".", tblName, structure, " ROW FORMAT SERDE '", whatsthis, "' WITH SERDEPROPERTIES('separatorChar'=',')", " STORED AS TEXTFILE LOCATION '", s3_data_path, "'")
  }
  
  cursor$execute(qry)
}




#' @export
athena.buildConnection = function(bucket = "s3://aws-athena-query-results-xxxxxxxxxxxx-ap-southeast-2/", 
                                  region = 'ap-southeast-2', profile = NULL, 
                                  aws_config_path = NULL
                                  ){
  pyathena = reticulate::import('pyathena')
  
  if(file.exists(aws_config_path %>% verify('character', lengths = 1, null_allowed = T))){
    aws_config_file             = aws_config_path %>% paste("config", sep = '/')
    aws_shared_credentials_file = aws_config_path %>% paste("credentials", sep = '/')
    Sys.setenv(AWS_CONFIG_FILE = aws_CONFIG_FILE, AWS_SHARED_CREDENTIALS_FILE = aws_shared_credentials_file)
  }
  
  pyathena$connect(s3_staging_dir = bucket, region_name = region)
}

#' @export
athena.read_s3   = function(con, query, max_rows = NULL){
  pandas   = reticulate::import('pandas')
  if(!is.null(max_rows)){
    nrow     = pandas$read_sql(query %>% sql.nrow, con)[1,1]
    banned   = nrow > max_rows
  } else banned = F
  if(banned) stop('The query will result a table with' %>% paste(nrow, 'rows which is higher than maximum:', max_rows))
  else pandas$read_sql(query, con)
}

#' @export
athena.buildDataset = function(conn, dsName = 'dataset'){
  cursor = conn$cursor()
  qry = paste("CREATE DATABASE", dsName)
  cursor$execute(qry)
}

# Example:
# acon = buildAthenaConnection()
# buildAthenaTable(acon, dsName = 'event', tblName = 'eventlogs', s3_data_path = datapath, drop_if_exists = T)

#' @export
sql.mlMapper = function(input, caseid_col, ts_col, et_col, var_col, value_col, tscat_col, variables){
  cvpr = paste0("select ", caseid_col, ", ", tscat_col, ", ", var_col, ", max_by(", value_col, ", ", ts_col, ") as latestValue from (", input, ") group by ", caseid_col, ", ", tscat_col, ", ", var_col)
  cvpr = sql.cast(cvpr, id_col = c(caseid_col, tscat_col), var_col = var_col, value_col = 'latestValue', variables = variables, aggregator = 'SUM')
  # cptime = paste0("select ", caseid_col, "\n")
  # cptime %<>% paste0(",MIN(", ts_col, ") AS caseStartTime", "\n")
  # cptime %<>% paste0(",MAX(", ts_col, ") AS latestEventTime", "\n")
  # cptime %<>% paste0(",MAX(IF(", et_col, " = 'LoanClosed', ", ts_col, ", cast('1900-01-01' as TIMESTAMP))) as closureTime", "\n")
  # cptime %<>% paste0(" from (", input, ") group by ", caseid_col)
  # sql.leftjoin(cvpr, cptime, by = caseid_col)
  cvpr
}

#' @export
sql.group_by = function(input, ...){
  by    = c(...)
  query = paste0("(", input, ") GROUP BY ", by %>% paste(collapse = ","))
  list(query = query, id_cols = by)
}

#' @export
sql.summarise = function(input, ...){
  if(inherits(input, 'list')){
    id_cols = input$id_cols
    input   = input$query
  } else {id_cols = ''}
  vars = list(...)
  if(length(vars) == 1 & inherits(vars[[1]], 'list')) vars = vars[[1]]
  vnms = names(vars)
  qry  = "SELECT "
  add  = id_cols %>% paste(collapse = ', ')
  if(add != ""){qry %<>% paste0(add, ", ")}
  
  for (vn in vnms){
    qry %<>% paste0(vars[[vn]]," AS ", vn)
    if(vn != vnms[length(vnms)]){
      qry  %<>% paste0(", \n")
    }
  }
  
  qry %>% paste0(" FROM ", input)
}

#' @export
sql.cast = function(input, id_col, var_col, value_col, variables, aggregator = 'SUM'){
  qry = paste0("SELECT ",  id_col %>% paste(collapse = ','),  ",", "\n")
  for (var in variables){
    qry %<>% paste0(aggregator, "(IF(variable = '", var, "',", value_col, ", NULL)) AS ", var, ", \n")
  }
  qry %>% paste0("count(", value_col, ") as Count from (", input, ")", " group by ", id_col %>% paste(collapse = ','))
}

# Example:
# athenaTableCast('event.eventlogs', 'caseid', 'variable', 'value', variables = c('OriginationChannel', 'Income'))

#' @export
sql.filter = function(input, ...){
  filter = list(...)
  if(length(filter) == 1 & inherits(filter[[1]], 'list')) filter = filter[[1]]
  fnms = names(filter)
  
  qry = paste0("SELECT * FROM (", input, ") WHERE ")
  nnn = length(fnms)
  for (i in sequence(nnn)){
    fn = fnms[i]
    qry %<>% paste0(fn," ", filter[[i]])
    
    if(i < nnn){qry %<>% paste0(" AND ")}
  }
  return(qry)
}


# Returns the case profile containing the latest values of each variable
# input must be in eventlog format:
sql.caseProfile.old = function(input, caseid_col, ts_col, et_col, var_col, value_col, variables, closure_event = 'HomeLoanClosed', with_times = T){
  cvpr = paste0("select ", caseid_col, ", ", var_col, ", max_by(", value_col, ", ", ts_col, ") as latestValue from (", input, ") group by ", caseid_col, ", ", var_col)
  cprf = sql.cast(cvpr, id_col = caseid_col, var_col = var_col, value_col = 'latestValue', variables = variables, aggregator = 'SUM')
  if(with_times){
    cptime = paste0("select ", caseid_col, "\n")
    cptime %<>% paste0(",MIN(", ts_col, ") AS firstEventTime", "\n")
    cptime %<>% paste0(",MAX(", ts_col, ") AS lastEventTime", "\n")
    cptime %<>% paste0(",MAX(IF(", et_col, " = '", closure_event, "', ", ts_col, ", cast('1900-01-01' as TIMESTAMP))) as closureTime", "\n")
    # cptime %<>% paste0(",closureTime - caseStartTime AS LoanAge", "\n")
    cptime %<>% paste0(" from (", input, ") group by ", caseid_col)
    return(sql.leftjoin(cprf, cptime, by = caseid_col))
  } else {return(cprf)}
}

#' @export
sql.caseProfile = function(
  input, 
  caseid_col    = 'caseid', 
  eventtime_col = 'eventtime', 
  eventtype_col = 'eventtype', 
  attribute_col = 'variable',
  value_col     = 'value', 
  first_event_type = F,
  first_event_time = F,
  last_event_type = F,
  last_event_time = F,
  
  first_event_times       = character(),
  last_event_times        = character(),
  first_event_attributes  = list(),
  last_event_attributes   = list(),
  
  first_attribute_times  = character(),
  last_attribute_times   = character(),
  
  first_attribute_values  = character(),
  last_attribute_values   = character(),
  
  sum_attribute_values    = character()
){
  
  script = "SELECT" %>% paste('  ','\n', caseid_col)
  
  if(first_event_type) {script %<>% paste0(", \n  MIN_BY(", eventtype_col, ", ", eventtime_col, ") AS firstEventType")}
  if(first_event_time) {script %<>% paste0(", \n  MIN(", eventtime_col, ") AS firstEventTime")}
  if(last_event_type)  {script %<>% paste0(", \n  MAX_BY(", eventtype_col, ", ", eventtime_col, ") AS lastEventType")}
  if(last_event_time)  {script %<>% paste0(", \n  MAX(", eventtime_col, ") AS lastEventTime")}

  for(event in first_event_times){
    script %<>% paste0(", \n  MIN(IF(", eventtype_col, " = '", event, "', ", eventtime_col, ", NULL)) as ", event, '_first_time', "\n")
  }

  for(attr in first_attribute_times){
    script %<>% paste0(", \n  MIN(IF(", attribute_col, " = '", attr, "', ", eventtime_col, ", NULL)) as ", attr, '_first_time', "\n")
  }

  for(event in last_event_times){
    script %<>% paste0(", \n  MAX(IF(", eventtype_col, " = '", event, "', ", eventtime_col, ", NULL)) as ", event, '_last_time', "\n")
  }
  
  for(attr in last_attribute_times){
    script %<>% paste0(", \n  MAX(IF(", attribute_col, " = '", attr, "', ", eventtime_col, ", NULL)) as ", attr, '_last_time', "\n")
  }
  
  for(event in first_event_attributes){
    for(attr in event$attributes){
      script %<>% paste0(",", "\n", "  ", "MIN_BY(", "\n", "  IF(", "  ", eventtype_col, " = '", event$type, "' AND ", attribute_col, " = '", attr, "'  ,", 
                         value_col, ", NULL), ", "\n  IF(", "  ", eventtype_col, " = '", event$type, "' AND ", attribute_col, " = '", attr, "'",  
                         "  ,", eventtime_col, ", NULL)) AS ", event$type, '_first_', attr, "\n")
    }
  }

  for(event in last_event_attributes){
    for(attr in event$attributes){
      script %>% 
        paste0(",", "\n", "  ", "MAX_BY(", "\n", "  IF(", eventtype_col, " = '", event$type, "' AND ", attribute_col, " = '", attr, "'  ,", 
               value_col, ", NULL), ", "\n  IF(", eventtype_col, " = '", event$type, "' AND ", attribute_col, " = '", attr, "'", "\n",  
               "  ,", eventtime_col, ", NULL)",") AS ", event$type, '_last_', attr, "\n")
    }
  }
  
  for(attr in first_attribute_values){
      script %<>% paste0(",", "\n", "  ", "MIN_BY(", "\n", "  IF(", attribute_col, " = '", attr, "'  ,", 
                         value_col, ", NULL), ", "\n  IF(", attribute_col, " = '", attr, "'  ,", 
                         eventtime_col, ", NULL)",") AS ", "first_", attr, "\n")
  }

  for(attr in last_attribute_values){
    script %<>% paste0(",", "\n", "  ", "MAX_BY(", "\n", "  IF(", attribute_col, " = '", attr, "'  ,", 
                       value_col, ", NULL), ", "\n  IF(", "\n", "  ", attribute_col, " = '", attr, "'", "\n",  
                       "  ,", eventtime_col, ", NULL)",") AS ", "last_", attr, "\n")
  }

  for(attr in sum_attribute_values){
    script %<>% paste0(",", "\n", "  ", "SUM(", "\n", "  IF(", attribute_col, " = '", attr, "'  ,", 
                       value_col, ", NULL)) AS ", "last_", attr, "\n")
  }
  
  # todo: can add more aggregator functions like mean, median, sum of latest n days, etc  ... 
  
  script %>% paste0("\n  ", "FROM (", input, ") GROUP BY ", caseid_col)
}


#' @export
sql.arrange = function(input, by){
  paste0(input, " ORDER BY ", by %>% paste(collapse = ","))
}

#' @export
sql.leftjoin = function(table1, table2, by){
  paste0("SELECT * FROM (", table1, ") a LEFT JOIN (", table2, ") b ON a.", by, " = b.", by)
}

#' @export
sql.select = function(input, ...){
  arg = list(...)
  if(length(arg) == 1 & inherits(arg[[1]], 'list')) arg = arg[[1]]
  anms = names(arg)
  
  qry = paste0("SELECT ")
  
  for (i in sequence(length(arg))){
    if(is.empty(anms[i])){
      qry %<>% paste0(arg[[i]])
    } else {
      qry %<>% paste0(arg[[i]], " AS ", anms[i])
    }
      
    if(i != length(arg)){qry %<>% paste0(", ")}
  }
  qry %>% paste0(" FROM (", input, ") ")
}

#' @export
sql.mutate = function(input, ...){
  arg = list(...)
  if(length(arg) == 1 & inherits(arg[[1]], 'list')) arg = arg[[1]]
  anms = names(arg)
  
  qry = paste0("SELECT *, ")
  
  for (a in anms){
    qry %<>% paste0(arg[[a]], " AS ", a)
    if(a != anms[length(anms)]){qry %<>% paste0(", ")}
  }
  qry %>% paste0(" FROM (", input, ") ")
}

# Example:
# qry = 'event.eventlogs' %>% sql.mutate(fvalue = "CAST(value as DOUBLE)")
# qry %<>% paste0(" where variable <> 'variable'")
# read_s3.athena(acon, query = qry %>% paste0(" limit 20")) %>% View

# qry = 'event.eventlogs' %>% sql.mutate(fvalue = "CAST(value as DOUBLE)")
# qry %<>% paste0(" where variable <> 'variable'")
# qry %<>% sql.mutate(status = "CASE WHEN fvalue > 1 THEN 'A' WHEN fvalue = 1 THEN 'C' ELSE 'B' END")
# qry = "SELECT * FROM event.eventlogs WHERE variable = 'ProductCode'"


# Example:
# sql.binColumn('LoanTenure', breaks = c('< 1 Yr' = 365, '1-2 Yrs' = 730, '2-3 Yrs' = 1095, '3-4 Yrs' = 1460, '4-5 Yrs' = 1825, '> 6 Yrs' = 2190))
#' @export
sql.binColumn = function(column, breaks){
  breaks %<>% sort
  mutscr = "CASE \n"
  nms = names(breaks)
  for (i in sequence(length(breaks))){
    if(i == 1){
      mutscr %<>% paste0("WHEN ", column, " < ", breaks[i], " THEN '", nms[i], "' \n")
    } else if (i < length(breaks)){
      mutscr %<>% paste0("WHEN ", column, " > ", breaks[i - 1], " AND ", column, " < ", breaks[i], " THEN '", nms[i], "' \n")
    } else {
      mutscr %<>% paste0("ELSE '", nms[i], "' END")
    }
  }  
  
  # CASE
  # WHEN Tenure < 0 THEN '0'
  # WHEN Tenure < 365 AND Tenure > 0 THEN '1'
  # WHEN Tenure < 730 AND Tenure > 365 THEN '2'
  # WHEN Tenure < 1095 AND Tenure > 730 THEN '3'
  # WHEN Tenure < 1460 AND Tenure > 1095 THEN '4'
  # WHEN Tenure < 1825 AND Tenure > 1460 THEN '5'
  # WHEN Tenure < 2190 AND Tenure > 1825 THEN '6'
  # ELSE '7'
  # END
  return(mutscr)
}

#' @export
sql.head = function(input, n = 6){
  'SELECT * from (' %>% paste(input, ') LIMIT', n)
}



#' @export
sql.nrow = function(input){
  "SELECT COUNT(*) FROM (" %>% paste0(input, ")")
}

#' @export
sql.cast = function(input, id_col, var_col, value_col, variables, aggregator = 'SUM'){
  qry = paste0("SELECT ",  id_col %>% paste(collapse = ','),  ",", "\n")
  for (var in variables){
    qry %<>% paste0(aggregator, "(IF(variable = '", var, "',", value_col, ", NULL)) AS ", var, ", \n")
  }
  qry %>% paste0("count(", value_col, ") as Count from (", input, ")", " group by ", id_col %>% paste(collapse = ','))
}


################## PARQUET TOOLS ###############################

parquet2calumns.old = function(path.parquet, path.columns, features){
  ons = list.files(path.parquet) %>% stringr::str_remove('.snappy.parquet')
  tns = list.files(path.columns) %>% stringr::str_remove('.rds')
  for (fn in (features %-% tns)){
    cat('Reading ', fn, '... ')
    out = NULL
    for(ns in ons){
      pfn = ns %++% '.snappy.parquet'
      out = arrow::read_parquet(path.parquet %++% pfn, col_select = fn) %>% rbind(out)
    }
    out %>% saveRDS(path.co1umns %>% pasteo(fn, '.rds'))
    # out %>% write.csv(path.co1umns %>% paste0(fn, .csv '), row.names = F)
    cat('Done! ' , '\n')
  }
}


# SLOW
#' @export
parquet2csv = function(path.parquet, path.csv, columns = NULL){
  ons = list.files(path.parquet) %>% stringr::str_remove('.snappy.parquet')
  tns = list.files(path.csv) %>% stringr::str_remove('.csv')
  for(ns in ons){
    if(!ns %in% tns){
      pfn = ns %++% ' snappy.parquet'
      
      cat('Reading ', pfn, '... ')
      arrow::read_parquet(path.parquet %++% pfn, col_select = columns) %>% write.csv(path.csv %++% ns %++% '.csv', row.names = F)
      cat('Done! ', '\n')
    }
  }
}

#' @export
parquet2RData = function(path.parquet, path.RData, columns = NULL){
  ons = list.files(path.parquet) %>% stringr::str_remove('.snappy.parquet')
  tns = list.files(path.RData) %>% stringr::str_remove('.RData')
  if(!file.exists(path.RData)) dir.create(path.RData)
  for(ns in ons){
    if(!ns %in% tns){
      pfn = ns %++% '.snappy.parquet'
      cat('Reading ', pfn, '... ')
          if(!is.null(columns)){
            tbl = arrow::read_parquet(path.parquet %++% pfn, col_select = columns)
          } else {
            tbl = arrow::read_parquet(path.parquet %++% pfn)
          }
          save(tbl, file = path.RData %++% ns %++% '.RData')
          cat('Done! ', '\n')
          gc()
          rm(list = 'tbl')
    }
  }
}


#' @export
parquet2DataFrame = function(path.parquet, columns = NULL, silent = T){
  ons = list.files(path.parquet) %-% '_SUCCESS'
  out = NULL
  for(pfn in ons){
      if(!silent) cat('\n', 'Reading ', pfn, '... ')
      if(!is.null(columns)){
        tbl = arrow::read_parquet(path.parquet %>% paste(pfn, sep = '/'), col_select = columns)
      } else {
        tbl = arrow::read_parquet(path.parquet %>% paste(pfn, sep = '/'))
      }
      out %<>% rbind(tbl)
  }
  return(out)
}

#' @export
RData2Columns = function(path.RData, path.columns, columns = NULL, buffer_size = 100){
  if(!file.exists(path.columns)) dir.create(path.columns)

  ons = list.files(path.RData, full.names = T)
  tns = list.files(path.columns) %>% stringr::str_remove('.RData')
  if(is.null(columns)){
    load(ons[1])
    columns = colnames(tbl)
  }
  columns %<>% setdiff(tns)
  cls = columns
  while (length(cls) > 0){
    buffer = cls %>% sample(size = min(buffer_size, length(cls))) 
    cls = cls %>% setdiff(buffer)
    out = NULL
    for (fn in ons){
      load(fn)
      out = tbl[buffer] %>% rbind(out)
    }
    # Write columns
    for(cn in colnames(out)){
      col = out %>% pull(cn)
      save(col, file = paste0(path.columns, cn, '.RData'))
    }
    gc()
  }
}


#' @export
parquet2RDS = function(path.parquet, path.rds, columns = NULL){
  ons = list.files(path.parquet) %>% stringr::str_remove('.snappy.parquet')
  tns = list.files(path.rds) %>% stringr::str_remove('.rds')
  if(!file.exists(path.rds)) dir.create(path.rds)
  for(ns in ons %>% setdiff(tns)){
    pfn = ns %++% '.snappy.parquet'
    cat('Reading', pfn, '... ')
        if(!is.null(columns)){
          tbl = arrow::read_parquet(path.parquet %++% pfn, col_select = columns)
        } else {
          tbl = arrow::read_parquet(path.parquet %++% pfn)
        }
        saveRDS(tbl, file = path.rds %++% ns %++% '.rds')
        cat('Done! ', '\n')
  }
}


#' @export
RDS2Columns = function(path.rds, path.columns, columns = NULL, buffer_size = 100){
  if(!file.exists(path.columns)) dir.create(path.columns)
  ons = list.files(path.rds, full.names = T)
  tns = list.files(path.columns) %>% stringr::str_remove('.rds')
  if(is.null(columns)){
    columns = readRDS(ons[1]) %% colnames
  }
  columns %<>% setdiff(tns)
  cls = columns
  while(length(cls) > 0){
    buffer = cls %>% sample(size = min(buffer_size, length(cls)))
    cls = cls %>% setdiff(buffer)
    tbl = NULL
    cut = cut + 1
    for(fn in ons){
      tbl = readRDS(fn)[buffer] %>% rbind(tbl)
    }
    # write columns
    for(cn in colnames(tbl)){
      tbl %>% pull(cn) %>% saveRDS(file = paste0(path.columms, cn,'.rds'))
    }
    gc()
  }
}


#' @export
read_table_from_columns.RData = function(path.columns, columns = NULL, filter = NULL){
  ons = list.files(path.columns, full.names = F) %>% stringr::str_remove('.RData')
  if(is.null(columns)){
    columns = ons
  } else {columns %<>% intersect(ons)}
  if(!is.null(filter)){
    fns = names(filter)
    assert(fns %<% ons, 'Filter contains non-existing columns!')
    boolind = T
    for(cn in names(filter)){
      load(paste0(path.columns, cn, '.RData'))
      if (!is.null(filter[[cn]]$max)) {boolind = boolind & (col <= filter[[cn]]$max)}
      if (!is.null(filter[[cn]]$min)) {boolind = boolind & (col >= filter[[cn]]$min)}
      if (!is.null(filter[[cn]]$domain)) {boolind = boolind & (col %in% filter[[cn]]$domain)}
    }
    ind= which(boolind)
  }
  out = data.frame()
  for(cn in columns){
    load(file = paste0(path.columns, cn, '.RData'))
    if(!is.null(filter)){col = col[ind]}
    if(inherits(col, 'integer64')){
      col %<>% as.integer
    }
    if(cn == columns[1]){
      out = data.frame(col)
      names(out) <- cn
    } else {
      out[cn] = col
    }
  }
  return(out)
}


