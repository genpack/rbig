
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
# 2.1.0     26 November 2026   Bugs fixed: row order of RDS2Columns()/RData2Columns() was reversed; RDS2Columns()
#                              failed (typos); column files now use the WIDETABLE layout ('.fvec' object in
#                              RData files); paths built with file.path(). parquet2WIDETABLE() added.
#                              parquet2RDS(), parquet2RData() and read_table_from_columns.RData() are no longer exported.

#' @include utils.R wt.R
NULL

################## HELPERS ###############################

#' List the parquet files of a folder
#'
#' @param path.parquet (character) folder (for example a folder written by Spark).
#' @return Full paths of the \code{*.parquet} files, sorted by name.
#' @keywords internal
list_parquet_files = function(path.parquet){
  assert(dir.exists(path.parquet), sprintf("Folder '%s' does not exist!", path.parquet))
  sort(list.files(path.parquet, pattern = '\\.parquet$', full.names = TRUE))
}

#' Check that package arrow is installed
#' @return \code{NULL}; raises an error if \pkg{arrow} is not installed.
#' @keywords internal
require_arrow = function(){
  assert(requireNamespace('arrow', quietly = TRUE), "Package 'arrow' is required to read parquet files. Install it with install.packages('arrow').")
}

#' Read some columns of a parquet file
#'
#' @param file (character) path of the parquet file.
#' @param columns (character) columns to read; \code{NULL} for all.
#' @return A \code{data.frame}.
#' @keywords internal
read_parquet_columns = function(file, columns = NULL){
  require_arrow()
  if (is.null(columns)) {
    tbl = arrow::read_parquet(file)
  } else {
    tbl = arrow::read_parquet(file, col_select = tidyselect::all_of(columns))
  }
  as.data.frame(tbl)
}

#' Column names of a parquet file (without reading the data)
#' @param file (character) path of the parquet file.
#' @return Character vector of column names.
#' @keywords internal
parquet_column_names = function(file){
  require_arrow()
  arrow::ParquetFileReader$create(file)$GetSchema()$names
}

#' Read the table stored in an RData file
#'
#' @param file (character) path of the RData file.
#' @return The object named \code{tbl} in the file, or the first object if there is no
#'   \code{tbl}.
#' @keywords internal
load_table_RData = function(file){
  env = new.env()
  nms = load(file, envir = env)
  env[[if ('tbl' %in% nms) 'tbl' else nms[1]]]
}

#' Write row chunks as one file per column
#'
#' Core of \code{\link{RDS2Columns}}, \code{\link{RData2Columns}} and
#' \code{\link{parquet2WIDETABLE}}. Columns are processed in buffers of
#' \code{buffer_size} columns: for each buffer, those columns are read from every chunk,
#' combined in the order of \code{files} and written to \code{path.columns} in the
#' \code{\link{WIDETABLE}} layout.
#'
#' @param files (character) chunk files, in row order.
#' @param reader function(file, columns) returning a \code{data.frame} with the given
#'   columns of a chunk.
#' @param all_columns (character) all column names available.
#' @param path.columns (character) output folder (created if needed).
#' @param columns (character) columns to write; \code{NULL} for all. Columns that already
#'   have a file in \code{path.columns} are skipped.
#' @param buffer_size (integer) number of columns per pass.
#' @param format (character) \code{'rds'} or \code{'RData'}.
#' @param verbose (logical) print progress?
#' @return Names of the columns written (invisibly).
#' @keywords internal
chunks_to_columns = function(files, reader, all_columns, path.columns, columns = NULL,
                             buffer_size = 100, format = c('rds', 'RData'), verbose = FALSE){
  format = match.arg(format)
  assert(length(files) > 0, 'No input file found!')
  if (!dir.exists(path.columns)) dir.create(path.columns, recursive = TRUE)
  existing = sub('\\.(rds|RData)$', '', list.files(path.columns, pattern = '\\.(rds|RData)$'))
  if (is.null(columns)) columns = all_columns
  bad = setdiff(columns, all_columns)
  assert(length(bad) == 0, paste('Unknown column(s):', paste(utils::head(bad, 10), collapse = ', ')))
  columns = setdiff(columns, existing)
  buffer_size = max(1, as.integer(buffer_size))
  buffers = split(columns, ceiling(seq_along(columns) / buffer_size))
  for (b in buffers){
    parts = lapply(files, function(f) reader(f, b))
    for (cn in b){
      pieces = lapply(parts, function(p) p[[cn]])
      vec = if (any(vapply(pieces, is.factor, logical(1)))) Reduce(wt_combine, pieces) else do.call(c, unname(pieces))
      fn  = file.path(path.columns, paste0(cn, '.', format))
      if (format == 'rds') {
        saveRDS(vec, fn)
      } else {
        .fvec = vec
        save(.fvec, file = fn)
      }
    }
    rm(parts)
    gc()
    if (verbose) message(length(b), ' column(s) written.')
  }
  invisible(columns)
}

################## PARQUET TOOLS ###############################

#' Read a folder of parquet files into one data.frame
#'
#' Reads every \code{*.parquet} file in \code{path.parquet} (the layout written by Spark)
#' with \pkg{arrow} and binds the rows in file-name order. Use it to load a table that
#' fits in memory, for example to feed it to \code{\link{as.WIDETABLE}} or
#' \code{rbind(wt, df)}. For tables that do not fit in memory use
#' \code{\link{parquet2WIDETABLE}}.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param columns (character) optional subset of columns to read.
#' @param silent (logical) if \code{FALSE}, prints the name of each file read.
#' @return A \code{data.frame}.
#' @examples
#' \dontrun{
#' df = parquet2DataFrame('data/customers.parquet', columns = c('id', 'age'))
#' }
#' @export
parquet2DataFrame = function(path.parquet, columns = NULL, silent = TRUE){
  files = list_parquet_files(path.parquet)
  parts = lapply(files, function(f){
    if (!silent) message('Reading ', basename(f), ' ...')
    read_parquet_columns(f, columns)
  })
  if (length(parts) == 0) return(data.frame())
  cols = names(parts[[1]])
  for (p in parts) assert(identical(sort(names(p)), sort(cols)), "All parquet files must have the same columns!")
  lst  = lapply(cols, function(cn) Reduce(wt_combine, lapply(parts, function(p) p[[cn]])))
  names(lst) = cols
  list_to_df(lst, sum(vapply(parts, nrow, integer(1))))
}

#' Build a WIDETABLE from a folder of parquet files
#'
#' Converts the parquet files of a folder (for example a table written by Spark) into a
#' \code{\link{WIDETABLE}} without loading the whole table into memory: columns are
#' processed \code{buffer_size} at a time, and each is read from every parquet file,
#' combined in file-name order and written to its own file. Interrupted conversions can
#' be resumed: columns already converted are skipped.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param name (character) name of the WIDETABLE (its folder is \code{<path>/<name>}).
#' @param path (character) existing parent folder of the WIDETABLE. Default \code{'.'}.
#' @param columns (character) columns to convert; \code{NULL} (default) for all.
#' @param buffer_size (integer) number of columns converted per pass. Default 100.
#'   Memory use is about \code{buffer_size} full columns.
#' @param format (character) \code{'rds'} (default) or \code{'RData'}.
#' @param overwrite (logical) delete an existing table of the same name first?
#'   Default \code{FALSE} (an existing conversion is resumed).
#' @param verbose (logical) print progress?
#' @param ... further fields passed to \code{WIDETABLE()}, e.g. \code{size_limit}.
#' @return A \code{WIDETABLE}.
#' @examples
#' \dontrun{
#' wt = parquet2WIDETABLE('data/features.parquet', name = 'features', path = 'tables')
#' dim(wt)
#' }
#' @export
parquet2WIDETABLE = function(path.parquet, name, path = '.', columns = NULL, buffer_size = 100,
                             format = c('rds', 'RData'), overwrite = FALSE, verbose = FALSE, ...){
  format = match.arg(format)
  files  = list_parquet_files(path.parquet)
  assert(length(files) > 0, sprintf("No parquet file found in '%s'!", path.parquet))
  folder = file.path(path, name)
  if (overwrite) unlink(folder, recursive = TRUE)
  written = chunks_to_columns(files, read_parquet_columns, parquet_column_names(files[1]), folder,
                              columns = columns, buffer_size = buffer_size, format = format, verbose = verbose)
  # make the WIDETABLE rescan the folder if columns were added:
  if (length(written) > 0) unlink(file.path(folder, '.meta.rds'))
  WIDETABLE(name = name, path = path, ...)
}

#' Convert a folder of parquet files to RData files
#'
#' Each \code{*.parquet} file is read with \pkg{arrow} and saved as an \code{.RData}
#' file (containing an object named \code{tbl}) with the same base name in
#' \code{path.RData}. Files already converted are skipped. The output can be split
#' into column files with \code{\link{RData2Columns}}.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param path.RData (character) output folder (created if needed).
#' @param columns (character) optional subset of columns to keep.
#' @return Paths of the files written (invisibly).
#' @keywords internal
parquet2RData = function(path.parquet, path.RData, columns = NULL){
  files = list_parquet_files(path.parquet)
  if (!dir.exists(path.RData)) dir.create(path.RData, recursive = TRUE)
  out = file.path(path.RData, paste0(sub('(\\.snappy)?\\.parquet$', '', basename(files)), '.RData'))
  for (k in seq_along(files)){
    if (file.exists(out[k])) next
    tbl = read_parquet_columns(files[k], columns)
    save(tbl, file = out[k])
    rm(tbl)
  }
  invisible(out)
}

#' Convert a folder of parquet files to RDS files
#'
#' Each \code{*.parquet} file is read with \pkg{arrow} and saved as an \code{.rds}
#' file with the same base name in \code{path.rds}. Files already converted are skipped.
#' The output can be split into column files with \code{\link{RDS2Columns}}.
#'
#' @param path.parquet (character) folder containing the parquet files.
#' @param path.rds (character) output folder (created if needed).
#' @param columns (character) optional subset of columns to keep.
#' @return Paths of the files written (invisibly).
#' @keywords internal
parquet2RDS = function(path.parquet, path.rds, columns = NULL){
  files = list_parquet_files(path.parquet)
  if (!dir.exists(path.rds)) dir.create(path.rds, recursive = TRUE)
  out = file.path(path.rds, paste0(sub('(\\.snappy)?\\.parquet$', '', basename(files)), '.rds'))
  for (k in seq_along(files)){
    if (file.exists(out[k])) next
    saveRDS(read_parquet_columns(files[k], columns), out[k])
  }
  invisible(out)
}

################## CHUNKS TO COLUMNS ###############################

#' Split RDS row chunks into one file per column
#'
#' Reads row chunks (each \code{.rds} file in \code{path.rds} holds a
#' \code{data.frame} with the same columns, files sorted by name give the row order)
#' and writes each column to its own file in \code{path.columns}, using the
#' \code{\link{WIDETABLE}} layout. The result can be opened with
#' \code{WIDETABLE(name = basename(path.columns), path = dirname(path.columns))}.
#' Columns are processed in buffers of \code{buffer_size} columns to bound memory use.
#'
#' @param path.rds (character) folder containing the chunk files.
#' @param path.columns (character) output folder (created if needed).
#' @param columns (character) columns to extract; default all columns of the first chunk.
#'   Columns already present in \code{path.columns} are skipped, so an interrupted run
#'   can be resumed.
#' @param buffer_size (integer) number of columns processed per pass. Default 100.
#' @param format (character) format of the column files: \code{'rds'} (default) or
#'   \code{'RData'}.
#' @return Names of the columns written (invisibly).
#' @examples
#' chunks = tempfile(); dir.create(chunks)
#' saveRDS(mtcars[1:16, ], file.path(chunks, 'part1.rds'))
#' saveRDS(mtcars[17:32, ], file.path(chunks, 'part2.rds'))
#' tables = tempfile(); dir.create(tables)
#' RDS2Columns(chunks, file.path(tables, 'cars'))
#' wt = WIDETABLE(name = 'cars', path = tables)
#' dim(wt)
#' unlink(c(chunks, tables), recursive = TRUE)
#' @export
RDS2Columns = function(path.rds, path.columns, columns = NULL, buffer_size = 100, format = c('rds', 'RData')){
  format = match.arg(format)
  assert(dir.exists(path.rds), sprintf("Folder '%s' does not exist!", path.rds))
  files = sort(list.files(path.rds, pattern = '\\.rds$', full.names = TRUE, ignore.case = TRUE))
  assert(length(files) > 0, sprintf("No .rds file found in '%s'!", path.rds))
  all_columns = if (is.null(columns)) colnames(readRDS(files[1])) else columns
  reader = function(f, cols) readRDS(f)[cols]
  chunks_to_columns(files, reader, all_columns, path.columns, columns, buffer_size, format)
}

#' Split RData row chunks into one file per column
#'
#' Like \code{\link{RDS2Columns}}, for chunks stored as \code{.RData} files (each holding
#' a \code{data.frame}, named \code{tbl} as written by \code{parquet2RData}, or as the
#' only object of the file).
#'
#' @param path.RData (character) folder containing the chunk files.
#' @param path.columns (character) output folder (created if needed).
#' @param columns (character) columns to extract; default all columns of the first chunk.
#'   Columns already present in \code{path.columns} are skipped.
#' @param buffer_size (integer) number of columns processed per pass. Default 100.
#' @param format (character) format of the column files: \code{'RData'} (default) or
#'   \code{'rds'}.
#' @return Names of the columns written (invisibly).
#' @export
RData2Columns = function(path.RData, path.columns, columns = NULL, buffer_size = 100, format = c('RData', 'rds')){
  format = match.arg(format)
  assert(dir.exists(path.RData), sprintf("Folder '%s' does not exist!", path.RData))
  files = sort(list.files(path.RData, pattern = '\\.RData$', full.names = TRUE, ignore.case = TRUE))
  assert(length(files) > 0, sprintf("No .RData file found in '%s'!", path.RData))
  all_columns = if (is.null(columns)) colnames(load_table_RData(files[1])) else columns
  reader = function(f, cols) load_table_RData(f)[cols]
  chunks_to_columns(files, reader, all_columns, path.columns, columns, buffer_size, format)
}

#' Read a table from a folder of per-column RData files
#'
#' Superseded by \code{\link{WIDETABLE}}, which reads the same folders lazily. Reads the
#' column files (each holding one vector) into a \code{data.frame}, optionally keeping
#' only the rows that satisfy \code{filter}. \code{integer64} columns are converted to
#' numeric.
#'
#' @param path.columns (character) folder containing one \code{.RData} file per column.
#' @param columns (character) columns to read; default all.
#' @param filter (list) optional named list of filters (names are column names); each
#'   filter may have elements \code{min}, \code{max} (inclusive) and \code{domain}.
#' @return A \code{data.frame}.
#' @keywords internal
read_table_from_columns.RData = function(path.columns, columns = NULL, filter = NULL){
  available = sub('\\.RData$', '', list.files(path.columns, pattern = '\\.RData$'))
  columns   = if (is.null(columns)) available else intersect(columns, available)
  rd = function(cn){
    v = load_table_RData(file.path(path.columns, paste0(cn, '.RData')))
    if (inherits(v, 'integer64')) v = as.numeric(v)
    v
  }
  ind = NULL
  if (!is.null(filter)){
    bad = setdiff(names(filter), available)
    assert(length(bad) == 0, paste('Filter contains non-existing columns:', paste(bad, collapse = ', ')))
    keep = TRUE
    for (cn in names(filter)){
      v = rd(cn)
      f = filter[[cn]]
      if (!is.null(f$max))    keep = keep & !is.na(v) & v <= f$max
      if (!is.null(f$min))    keep = keep & !is.na(v) & v >= f$min
      if (!is.null(f$domain)) keep = keep & v %in% f$domain
    }
    ind = which(keep)
  }
  lst = lapply(columns, function(cn){v = rd(cn); if (is.null(ind)) v else v[ind]})
  names(lst) = columns
  list_to_df(lst, if (length(lst) == 0) 0 else length(lst[[1]]))
}
