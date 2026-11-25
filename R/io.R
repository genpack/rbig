
# Header
# Filename:      io.R
# Description:   Column/file I/O tools that feed WIDETABLEs: converting parquet files into
#                row chunks (RDS/RData) and splitting row chunks into one file per column.
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    28 October 2016

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.0     28 October 2016    Initial Issue: Started with function readODBC().
# 1.0.0     26 June 2019       spark and athena tools and AGGERGATOR module added.
# 1.0.1     17 July 2019       Function sqlFilter() modified: small bug fixed.
# 2.0.0     26 November 2026   Package renamed to rwidetable: SQL, Spark, Athena and ODBC helpers removed;
#                              only the parquet/RDS/RData-to-columns tools that feed WIDETABLEs are kept.

################## PARQUET TOOLS ###############################

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


