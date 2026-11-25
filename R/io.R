
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

#' Valid filter elements and field types
#'
#' \code{valid.filter.elements} lists the names allowed in a filter list passed to
#' \code{\link{sqlFilter}} and \code{\link{rScriptFilter}}. \code{valid.field.types}
#' lists the values allowed for the \code{type} element of a filter.
#' @keywords internal
#' @noRd
valid.filter.elements = c('domain', 'min', 'max', 'type', 'na.rm', 'equal', 'query')
valid.field.types     = c('nominal', 'numeric', 'date', 'time')

#' Build an SQL \code{WHERE} condition for one column
#'
#' Converts a filter specification (a named list) into the text of an SQL condition
#' that can be used in a \code{WHERE} clause.
#'
#' @param colName (character) name of the column the filter applies to.
#' @param filter (list) filter specification. Allowed elements are:
#' \describe{
#'   \item{\code{type}}{one of \code{'nominal'} (default), \code{'numeric'}, \code{'date'}
#'     or \code{'time'}.}
#'   \item{\code{domain}}{vector of allowed values (\code{nominal} and \code{numeric} types).}
#'   \item{\code{min}, \code{max}}{lower and upper bounds (\code{numeric}, \code{date} and
#'     \code{time} types).}
#'   \item{\code{equal}}{(logical) if \code{TRUE}, bounds are inclusive (\code{>=}, \code{<=}).
#'     Default \code{FALSE}.}
#'   \item{\code{na.rm}}{(logical) if \code{TRUE}, adds \code{IS NOT NULL}. Default \code{FALSE}.}
#'   \item{\code{query}}{(character) a sub-query; if given, the condition is
#'     \code{colName IN (query)} and other elements are ignored.}
#' }
#' @param vf (logical) if \code{TRUE} (default), the filter is verified before use.
#' @return (character) the SQL condition, or \code{""} if the filter imposes no condition.
#' @seealso \code{\link{rScriptFilter}}, \code{\link{sqlScript}}
#' @examples
#' \dontrun{
#' sqlFilter('age', list(type = 'numeric', min = 18, max = 65, equal = TRUE))
#' # "age >= 18 AND age <= 65"
#' sqlFilter('state', list(domain = c('NSW', 'VIC')))
#' # "state IN ('NSW','VIC')"
#' }
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

#' Build an R logical expression for one column
#'
#' The R counterpart of \code{\link{sqlFilter}}: converts a filter specification into
#' the text of an R expression that evaluates to a logical vector, for example to be
#' used with \code{dplyr::filter} after \code{parse()}.
#'
#' @inheritParams sqlFilter
#' @return (character) the R expression, or \code{""} if the filter imposes no condition.
#' @seealso \code{\link{sqlFilter}}
#' @examples
#' \dontrun{
#' rScriptFilter('age', list(type = 'numeric', min = 18, na.rm = TRUE))
#' # "age > 18 & !is.na(age)"
#' }
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


#' Generate an SQL \code{SELECT} query
#'
#' Builds a complete \code{SELECT ... FROM dbName.tableName WHERE ...} query.
#'
#' @param tableName (character) name of the table.
#' @param fields (character) columns to select. Default \code{'*'}. If the vector is named,
#'   non-empty names are used as aliases (\code{field AS name}).
#' @param dbName (character) name of the database (schema) containing the table.
#' @param filter (list) named list of filters; each name is a column name and each
#'   element is a filter specification as described in \code{\link{sqlFilter}}.
#'   Conditions are combined with \code{AND}.
#' @param vf (logical) if \code{TRUE} (default), arguments are verified.
#' @return (character) the SQL query.
#' @examples
#' \dontrun{
#' sqlScript('customers', fields = c(id = 'cust_id', 'age'), dbName = 'sales',
#'           filter = list(age = list(type = 'numeric', min = 18)))
#' }
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


#' Read a table through ODBC
#'
#' Generates a query with \code{\link{sqlScript}}, runs it through an ODBC connection
#' (package \pkg{RODBC}) and returns the result.
#'
#' @inheritParams sqlScript
#' @param dsn (character) ODBC data source name.
#' @param ... further arguments passed to \code{RODBC::odbcConnect} and \code{RODBC::sqlQuery}.
#' @return A \code{data.frame} with the query result.
#' @keywords internal
readODBC <- function(tableName, fields = NULL, filter = NULL, dbName, dsn, vf = T, ...){
  # Verifications:
  if(vf){assert(require(RODBC), "Package 'RODBC' is not installed!", err_src = match.call()[[1]])}
  # todo: check arguments passed to odbcConnect
  channel    = odbcConnect(dsn = dsn, ...)
  D          = sqlQuery(channel = channel, query = sqlScript(tableName = tableName, fields = fields, dbName = dbName, filter = filter, vf = vf), ...)
  close(channel)
  return(D)
}

#' Run an SQL query through ODBC
#'
#' Opens an ODBC connection (package \pkg{RODBC}), runs the query, closes the
#' connection and returns the result.
#'
#' @param query (character) SQL query to run.
#' @param ... arguments passed to \code{RODBC::odbcConnect}, for example \code{dsn}.
#' @return A \code{data.frame} with the query result.
#' @examples
#' \dontrun{
#' runSQL("SELECT * FROM sales.customers", dsn = 'MyDSN')
#' }
#' @export
runSQL <- function(query, ...){
  channel    = odbcConnect(...)
  D          = sqlQuery(channel = channel, query = query)
  close(channel)
  return(D)
}

################## SPARK TOOLS ###############################

#' Connect to a local Spark instance with memory settings
#'
#' @param mem.exec (character) \code{spark.executor.memory}.
#' @param mem.shell_driver (character) \code{sparklyr.shell.driver-memory}.
#' @param mem.shell.exec (character) \code{sparklyr.shell.executor-memory}.
#' @param mem.max_result (character) \code{spark.driver.maxResultSize}.
#' @param mem.yarn_exec_overhead (character) \code{spark.yarn.executor.memoryOverhead}.
#' @return A \code{spark_connection} object (package \pkg{sparklyr}).
#' @keywords internal
spark.local = function(mem.exec = "64G", mem.shell_driver = "64G", mem.shell.exec = "64G", mem.max_result = "4G", mem.yarn_exec_overhead = "16G"){  
  config <- spark_config()
  config$spark.executor.memory <- mem.exec
  config[['sparklyr.shell.driver-memory']] <- mem.shell_driver
  config[['sparklyr.shell.executor-memory']] <- mem.shell.exec
  config[['spark.driver.maxResultSize']] <- mem.max_result
  config$spark.yarn.executor.memoryOverhead <- mem.yarn_exec_overhead
  spark_connect(master = "local", config = config)
}


#' Read a CSV file into a local Spark session
#'
#' Connects to a local Spark instance and reads a CSV file (local path or S3 URI)
#' as a Spark table named \code{'el'}.
#'
#' @param path (character) path or URI of the CSV file(s).
#' @return A \code{tbl_spark} object.
#' @export
spark.read_s3 = function(path){
  sc <- sparklyr::spark_connect(master = 'local')
  el <- sparklyr::spark_read_csv(sc, name = 'el', path = path)
  return(el)
}


#' @title Column Binner for Spark
#' @description Bins numeric columns of a given table and creates a new column containing categorical bins
#' @param df (dataframe, tibble, tbl_spark) table to be binned
#' @param binners (list) list containing:
#' \code{input_col (character)}: colname to be binned, must have numeric values
#' \code{output_col (character)}: column name to be generated as a result of binning
#' \code{ splits (named numeric)}: named numeric vector containing splits and labels for them
#' @return The input table with the binned columns added.
#' @examples
#' \dontrun{
#' df = data.frame(age = c(5, 25, 70))
#' spark.bin(df, list(list(input_col = 'age', output_col = 'age_band',
#'                         splits = c(young = 0, adult = 18, senior = 65, old = Inf))))
#' }
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
#' Rename columns using character arguments
#'
#' A standard-evaluation wrapper around \code{dplyr::rename} that works with
#' \code{data.frame}s and Spark tables.
#'
#' @param tbl a \code{data.frame}, tibble or \code{tbl_spark}.
#' @param ... named character values in the form \code{new_name = 'old_name'}.
#' @return The table with renamed columns.
#' @examples
#' \dontrun{
#' spark.rename(mtcars, cylinders = 'cyl')
#' }
#' @export
spark.rename = function(tbl, ...){
  ns = c(...) %>% verify('character')
  lb = names(ns)
  if(is.null(lb)){return(tbl)}
  scr = paste0("tbl %>% dplyr::rename(", lb %>% paste(ns, sep = ' = ') %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' Add columns using character expressions
#'
#' A standard-evaluation wrapper around \code{dplyr::mutate} that works with
#' \code{data.frame}s and Spark tables.
#'
#' @param tbl a \code{data.frame}, tibble or \code{tbl_spark}.
#' @param ... named character values in the form \code{new_column = 'expression'}.
#' @return The table with the new columns.
#' @examples
#' \dontrun{
#' spark.mutate(mtcars, power_per_cyl = 'hp / cyl')
#' }
#' @export
spark.mutate = function(tbl, ...){
  ns = c(...) %>% verify('character')
  lb = names(ns)
  if(is.null(lb)){return(tbl)}
  scr = paste0("tbl %>% dplyr::mutate(", lb %>% paste(ns, sep = ' = ') %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' Select columns by name
#'
#' A standard-evaluation wrapper around \code{dplyr::select}. Names not found in the
#' table are ignored.
#'
#' @param tbl a \code{data.frame}, tibble or \code{tbl_spark}.
#' @param ... character column names.
#' @return The table restricted to the given columns.
#' @examples
#' \dontrun{
#' spark.select(mtcars, 'mpg', 'cyl')
#' }
#' @export
spark.select = function(tbl, ...){
  ns = c(...) %>% verify('character') %>% intersect(colnames(tbl))
  if(length(ns) == 0){return(tbl[c()])}
  scr = paste0("tbl %>% dplyr::select(", ns %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' Drop columns by name
#'
#' A standard-evaluation wrapper around \code{dplyr::select(-col)}. Names not found
#' in the table are ignored.
#'
#' @param tbl a \code{data.frame}, tibble or \code{tbl_spark}.
#' @param ... character column names to drop.
#' @return The table without the given columns.
#' @examples
#' \dontrun{
#' spark.unselect(mtcars, 'mpg')
#' }
#' @export
spark.unselect = function(tbl, ...){
  ns = c(...) %>% verify('character') %>% intersect(colnames(tbl))
  if(length(ns) == 0){return(tbl)}
  scr = paste0("tbl %>% dplyr::select(", paste('-',ns) %>% paste(collapse = ' , '), ")")
  parse(text = scr) %>% eval
}

#' One-hot encode categorical columns of a Spark table
#'
#' Converts each given column to character, indexes it with
#' \code{sparklyr::ft_string_indexer} and expands it into one indicator column per
#' category (named \code{<column>_<category>}) with \code{sparklyr::ft_one_hot_encoder}.
#' Columns with two or fewer categories are left as they are.
#'
#' @param tbl a \code{tbl_spark}.
#' @param ... character names of the categorical columns.
#' @param keep_original (logical) keep the original columns? Default \code{TRUE}.
#' @param keep_index (logical) keep the intermediate \code{<column>_Index} columns?
#'   Default \code{FALSE}.
#' @return The Spark table with the indicator columns added.
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

#' Create an external Athena table over data in S3
#'
#' Runs a \code{CREATE EXTERNAL TABLE} statement through a \pkg{pyathena} connection.
#'
#' @param conn a \pkg{pyathena} connection, as returned by \code{\link{athena.buildConnection}}.
#' @param dsName (character) name of the Athena database.
#' @param tblName (character) name of the table to create.
#' @param column_types (named character) column types (for example
#'   \code{c(caseid = 'STRING', value = 'FLOAT')}); names are column names.
#' @param id_columns (character) names of the partition columns.
#' @param s3_data_path (character) S3 location of the data.
#' @param drop_if_exists (logical) drop an existing table with the same name first?
#'   Default \code{TRUE}.
#' @param format (character) data format: \code{'parquet'} (default) or \code{'csv'}.
#' @return Called for its side effect; returns the \pkg{pyathena} cursor result invisibly.
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




#' Open a connection to AWS Athena
#'
#' Uses the Python package \pkg{pyathena} through \pkg{reticulate}.
#'
#' @param bucket (character) S3 staging directory for query results
#'   (for example \code{'s3://my-bucket/athena/'}).
#' @param region (character) AWS region name.
#' @param profile (character) optional AWS profile name.
#' @param aws_config_path (character) optional folder containing the AWS \code{config}
#'   and \code{credentials} files. If given, environment variables
#'   \code{AWS_CONFIG_FILE} and \code{AWS_SHARED_CREDENTIALS_FILE} are set accordingly.
#' @param ... further arguments passed to \code{pyathena.connect}.
#' @return A \pkg{pyathena} connection object.
#' @examples
#' \dontrun{
#' con = athena.buildConnection('s3://my-bucket/athena/', region = 'ap-southeast-2')
#' }
#' @export
athena.buildConnection = function(bucket, 
                                  region, profile = NULL, 
                                  aws_config_path = NULL, ...
                                  ){
  pyathena = reticulate::import('pyathena')
  
  if(file.exists(aws_config_path %>% verify('character', lengths = 1, null_allowed = T))){
    aws_config_file             = aws_config_path %>% paste("config", sep = '/')
    aws_shared_credentials_file = aws_config_path %>% paste("credentials", sep = '/')
    Sys.setenv(AWS_CONFIG_FILE = aws_CONFIG_FILE, AWS_SHARED_CREDENTIALS_FILE = aws_shared_credentials_file)
  }
  
  pyathena$connect(s3_staging_dir = bucket, region_name = region, profile = profile, ...)
}

#' Run an Athena query and return the result as a data.frame
#'
#' @param con a \pkg{pyathena} connection.
#' @param query (character) SQL query.
#' @param max_rows (integer) optional limit. If given, the number of rows of the result
#'   is counted first and an error is raised if it exceeds \code{max_rows}.
#' @return A \code{data.frame} (converted from a pandas DataFrame).
#' @examples
#' \dontrun{
#' athena.read_s3(con, sql.head('SELECT * FROM event.eventlogs', 10))
#' }
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

#' Create an Athena database
#'
#' @param conn a \pkg{pyathena} connection.
#' @param dsName (character) name of the database to create.
#' @return Called for its side effect.
#' @export
athena.buildDataset = function(conn, dsName = 'dataset'){
  cursor = conn$cursor()
  qry = paste("CREATE DATABASE", dsName)
  cursor$execute(qry)
}

# Example:
# acon = buildAthenaConnection()
# buildAthenaTable(acon, dsName = 'event', tblName = 'eventlogs', s3_data_path = datapath, drop_if_exists = T)

#' Build a case-by-time-category feature table from an event log (SQL)
#'
#' For each case, time category and variable, takes the latest value (by timestamp)
#' and pivots the variables into columns with \code{\link{sql.cast}}.
#'
#' @param input (character) SQL query or table name of the event log.
#' @param caseid_col,ts_col,et_col,var_col,value_col,tscat_col (character) names of the
#'   case id, timestamp, event type, variable, value and time-category columns.
#' @param variables (character) variables to pivot into columns.
#' @return (character) SQL query.
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

#' Add a \code{GROUP BY} clause to a query
#'
#' Intended to be piped into \code{\link{sql.summarise}}.
#'
#' @param input (character) SQL query or table name.
#' @param ... character names of the grouping columns.
#' @return A list with elements \code{query} (character) and \code{id_cols}
#'   (the grouping columns).
#' @examples
#' 'sales.orders' %>% sql.group_by('customer') %>% sql.summarise(total = 'SUM(amount)')
#' @export
sql.group_by = function(input, ...){
  by    = c(...)
  query = paste0("(", input, ") GROUP BY ", by %>% paste(collapse = ","))
  list(query = query, id_cols = by)
}

#' Build an aggregating \code{SELECT} query
#'
#' @param input (character) SQL query or table name, or the list returned by
#'   \code{\link{sql.group_by}}.
#' @param ... named character aggregation expressions (\code{alias = 'SUM(x)'}), or a
#'   single named list of them.
#' @return (character) SQL query.
#' @examples
#' sql.summarise('sales.orders', n = 'COUNT(*)', total = 'SUM(amount)')
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

#' Pivot a long table into a wide table (SQL)
#'
#' Builds a query that turns rows of (\code{id_col}, \code{var_col}, \code{value_col})
#' into one column per variable, aggregating with \code{aggregator}. A \code{Count}
#' column with the number of non-null values per id is added.
#'
#' @param input (character) SQL query or table name of the long table.
#' @param id_col (character) id column(s) to group by.
#' @param var_col (character) column containing the variable names.
#' @param value_col (character) column containing the values.
#' @param variables (character) variables to turn into columns.
#' @param aggregator (character) SQL aggregate function. Default \code{'SUM'}.
#' @return (character) SQL query.
#' @examples
#' sql.cast('event.eventlogs', 'caseid', 'variable', 'value', variables = c('Income', 'Age'))
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

#' Wrap a query in a filtering \code{SELECT}
#'
#' @param input (character) SQL query or table name.
#' @param ... named character conditions in the form \code{column = "> 5"}: each
#'   condition is written as \code{column <condition>}; conditions are combined with
#'   \code{AND}. A single named list is also accepted.
#' @return (character) SQL query.
#' @examples
#' sql.filter('sales.orders', amount = '> 100', status = "= 'paid'")
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


#' Case profile from an event log (legacy version)
#'
#' Superseded by \code{\link{sql.caseProfile}}. Returns the latest value of each
#' variable per case, optionally joined with first/last event times and closure time.
#' @keywords internal
#' @noRd
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

#' Build a case profile query from an event log (SQL)
#'
#' Aggregates an event log (one row per event attribute: case id, event time, event
#' type, attribute name and value) into one row per case, with the requested summary
#' columns. Uses Presto/Athena functions \code{MIN_BY} and \code{MAX_BY}.
#'
#' @param input (character) SQL query or table name of the event log.
#' @param caseid_col,eventtime_col,eventtype_col,attribute_col,value_col (character)
#'   names of the case id, event time, event type, attribute and value columns.
#' @param first_event_type,first_event_time,last_event_type,last_event_time (logical)
#'   add columns \code{firstEventType}, \code{firstEventTime}, \code{lastEventType},
#'   \code{lastEventTime}?
#' @param first_event_times,last_event_times (character) event types for which the
#'   first/last time of occurrence is added (\code{<event>_first_time},
#'   \code{<event>_last_time}).
#' @param first_event_attributes,last_event_attributes (list) each element is a list with
#'   \code{type} (event type) and \code{attributes} (character); adds the value of each
#'   attribute at the first/last event of that type (\code{<type>_first_<attr>},
#'   \code{<type>_last_<attr>}).
#' @param first_attribute_times,last_attribute_times (character) attributes for which
#'   the first/last time they were recorded is added.
#' @param first_attribute_values,last_attribute_values (character) attributes for which
#'   the first/last recorded value is added (\code{first_<attr>}, \code{last_<attr>}).
#' @param sum_attribute_values (character) attributes whose values are summed.
#' @return (character) SQL query.
#' @examples
#' sql.caseProfile('event.eventlogs', last_event_time = TRUE,
#'                 last_attribute_values = c('Income', 'Balance'))
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


#' Add an \code{ORDER BY} clause
#'
#' @param input (character) SQL query.
#' @param by (character) columns to order by (\code{'col DESC'} is allowed).
#' @return (character) SQL query.
#' @examples
#' sql.arrange('SELECT * FROM sales.orders', c('customer', 'amount DESC'))
#' @export
sql.arrange = function(input, by){
  paste0(input, " ORDER BY ", by %>% paste(collapse = ","))
}

#' Left-join two queries
#'
#' @param table1,table2 (character) SQL queries (or \code{SELECT * FROM table}).
#' @param by (character) name of the join column (present in both).
#' @return (character) SQL query.
#' @examples
#' sql.leftjoin('SELECT * FROM a', 'SELECT * FROM b', by = 'id')
#' @export
sql.leftjoin = function(table1, table2, by){
  paste0("SELECT * FROM (", table1, ") a LEFT JOIN (", table2, ") b ON a.", by, " = b.", by)
}

#' Build a \code{SELECT} query with optional aliases
#'
#' @param input (character) SQL query or table name.
#' @param ... character expressions to select; named arguments become aliases
#'   (\code{alias = 'expression'}). A single list is also accepted.
#' @return (character) SQL query.
#' @examples
#' sql.select('sales.orders', 'customer', total = 'amount * 1.1')
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

#' Add computed columns to a query
#'
#' @param input (character) SQL query or table name.
#' @param ... named character expressions (\code{new_column = 'expression'}).
#'   A single named list is also accepted.
#' @return (character) SQL query: \code{SELECT *, expr AS name ... FROM (input)}.
#' @examples
#' sql.mutate('event.eventlogs', fvalue = 'CAST(value AS DOUBLE)')
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
#' Build an SQL \code{CASE} expression that bins a numeric column
#'
#' @param column (character) name of the numeric column.
#' @param breaks (named numeric) upper bounds of the bins; names are the bin labels.
#'   The last bin is the \code{ELSE} branch.
#' @return (character) SQL \code{CASE ... END} expression, usable in
#'   \code{\link{sql.mutate}}.
#' @examples
#' sql.binColumn('tenure', c('< 1 Yr' = 365, '1-2 Yrs' = 730, '> 2 Yrs' = Inf))
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

#' Limit a query to its first rows
#'
#' @param input (character) SQL query.
#' @param n (integer) maximum number of rows. Default 6.
#' @return (character) SQL query.
#' @examples
#' sql.head('SELECT * FROM sales.orders', 10)
#' @export
sql.head = function(input, n = 6){
  'SELECT * from (' %>% paste(input, ') LIMIT', n)
}



#' Count the rows of a query
#'
#' @param input (character) SQL query.
#' @return (character) SQL query returning a single count.
#' @examples
#' sql.nrow('SELECT * FROM sales.orders')
#' @export
sql.nrow = function(input){
  "SELECT COUNT(*) FROM (" %>% paste0(input, ")")
}

# Duplicate definition of sql.cast(); this one overrides the one above.
#' @rdname sql.cast
#' @export
sql.cast = function(input, id_col, var_col, value_col, variables, aggregator = 'SUM'){
  qry = paste0("SELECT ",  id_col %>% paste(collapse = ','),  ",", "\n")
  for (var in variables){
    qry %<>% paste0(aggregator, "(IF(variable = '", var, "',", value_col, ", NULL)) AS ", var, ", \n")
  }
  qry %>% paste0("count(", value_col, ") as Count from (", input, ")", " group by ", id_col %>% paste(collapse = ','))
}


################## PARQUET TOOLS ###############################

#' Convert parquet files to one RDS file per column (legacy, broken)
#'
#' Legacy draft superseded by \code{\link{parquet2RDS}} + \code{\link{RDS2Columns}}.
#' @keywords internal
#' @noRd
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
#' Convert a folder of parquet files to CSV files
#'
#' Each \code{*.snappy.parquet} file in \code{path.parquet} is written as a CSV file
#' with the same base name in \code{path.csv}. Files already converted are skipped.
#' Requires package \pkg{arrow}.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param path.csv (character) output folder.
#' @param columns (character) optional subset of columns to keep.
#' @return Called for its side effect (files written).
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

#' Convert a folder of parquet files to RData files
#'
#' Each \code{*.snappy.parquet} file is read with \pkg{arrow} and saved as an
#' \code{.RData} file (containing an object named \code{tbl}) in \code{path.RData}.
#' Files already converted are skipped.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param path.RData (character) output folder (created if needed).
#' @param columns (character) optional subset of columns to keep.
#' @return Called for its side effect (files written).
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


#' Read a folder of parquet files into one data.frame
#'
#' Reads every file in \code{path.parquet} (except \code{_SUCCESS}) with \pkg{arrow}
#' and binds the rows. This is the usual layout written by Spark.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param columns (character) optional subset of columns to read.
#' @param silent (logical) if \code{FALSE}, prints the name of each file read.
#' @return A tibble / \code{data.frame}.
#' @examples
#' \dontrun{
#' df = parquet2DataFrame('data/customers.parquet', columns = c('id', 'age'))
#' }
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
    if(is.null(out)){
      out = tbl
    } else {
      out %<>% dplyr::bind_rows(tbl)
    }
  }
  return(out)
}

#' Split a set of RData table chunks into one file per column
#'
#' Reads the row-chunks written by \code{\link{parquet2RData}} and writes each column
#' to its own \code{.RData} file in \code{path.columns}. Columns are processed in
#' buffers of \code{buffer_size} columns to bound memory use.
#'
#' @param path.RData (character) folder containing the chunk files.
#' @param path.columns (character) output folder (created if needed).
#' @param columns (character) columns to extract; default all columns of the first chunk.
#'   Columns already present in \code{path.columns} are skipped.
#' @param buffer_size (integer) number of columns processed per pass. Default 100.
#' @return Called for its side effect (files written).
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


#' Convert a folder of parquet files to RDS files
#'
#' Each \code{*.snappy.parquet} file is read with \pkg{arrow} and saved as an
#' \code{.rds} file with the same base name. Files already converted are skipped.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param path.rds (character) output folder (created if needed).
#' @param columns (character) optional subset of columns to keep.
#' @return Called for its side effect (files written).
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


#' Split a set of RDS table chunks into one file per column
#'
#' Reads the row-chunks written by \code{\link{parquet2RDS}} and writes each column
#' to its own \code{.rds} file in \code{path.columns}. Columns are processed in
#' buffers of \code{buffer_size} columns to bound memory use.
#'
#' @param path.rds (character) folder containing the chunk files.
#' @param path.columns (character) output folder (created if needed).
#' @param columns (character) columns to extract; default all columns of the first chunk.
#'   Columns already present in \code{path.columns} are skipped.
#' @param buffer_size (integer) number of columns processed per pass. Default 100.
#' @return Called for its side effect (files written).
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


#' Read a table from a folder of per-column RData files
#'
#' Reads the column files written by \code{\link{RData2Columns}} into a
#' \code{data.frame}, optionally filtering rows. \code{integer64} columns are
#' converted to integer.
#'
#' @param path.columns (character) folder containing one \code{.RData} file per column.
#' @param columns (character) columns to read; default all.
#' @param filter (list) optional named list of filters (names are column names); each
#'   filter may have elements \code{min}, \code{max} (inclusive) and \code{domain}.
#' @return A \code{data.frame}.
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


