# Header
# Filename:       wt.R
# Description:    WIDETABLE: a reference class for tables with (virtually) unlimited columns.
#                 Each column is stored in its own file; columns are loaded lazily and a
#                 size-limited in-memory cache keeps the most recently used columns.

# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.4     13 October 2020    Initial Issue
# 1.0.5     13 October 2020    generic methods exported
# 1.0.7     07 June 2021       Minor bug in extract.dataframe() fixed: changed order of columns in the output before fix
# 1.1.0     26 November 2026   Rewritten for speed and reliability:
#                              - table metadata persisted in '<path>/<name>/.meta.rds', so opening a table
#                                no longer reads every column file (legacy folders are scanned once)
#                              - set_value() on a row subset no longer overwrites column files with the subset
#                              - atomic file writes, multi-class columns (POSIXct), lazy loading fixes
#                              - rbind (with fill), cbind, left join, remove/rename columns, head/tail,
#                                dplyr select/pull methods, block-wise correlation

#' @include utils.R
NULL

#' Empty meta table of a WIDETABLE
#' @return A \code{data.frame} with columns \code{column}, \code{class},
#'   \code{n_unique} and \code{memsize} and no rows.
#' @keywords internal
wt_empty_meta = function(){
  data.frame(column = character(0), class = character(0), n_unique = numeric(0),
             memsize = numeric(0), stringsAsFactors = FALSE)
}

#' Combine two column vectors (used for row binding)
#'
#' Like \code{c()}, but factors are combined into a factor whose levels are the union
#' of the levels, and \code{POSIXlt} is converted to \code{POSIXct}.
#'
#' @param a,b vectors.
#' @return The combined vector.
#' @keywords internal
wt_combine = function(a, b){
  if (inherits(a, 'POSIXlt')) a = as.POSIXct(a)
  if (inherits(b, 'POSIXlt')) b = as.POSIXct(b)
  if (is.factor(a) || is.factor(b)){
    la = if (is.factor(a)) levels(a) else unique(as.character(a[!is.na(a)]))
    lb = if (is.factor(b)) levels(b) else unique(as.character(b[!is.na(b)]))
    return(factor(c(as.character(a), as.character(b)), levels = union(la, lb)))
  }
  if (length(a) == 0) return(b)
  if (length(b) == 0) return(a)
  out = c(a, b)
  # c() drops the time zone of POSIXct in some R versions:
  if (inherits(a, 'POSIXct') && !is.null(attr(a, 'tzone'))) attr(out, 'tzone') = attr(a, 'tzone')
  out
}

#' A typed vector of missing values
#'
#' @param proto a vector whose type/class is used.
#' @param n (integer) length of the result.
#' @return A vector of \code{n} \code{NA}s of the same class as \code{proto}.
#' @keywords internal
wt_na_like = function(proto, n){
  if (inherits(proto, 'POSIXlt')) proto = as.POSIXct(proto)
  proto[rep(NA_integer_, n)]
}

################## WIDE TABLES: ##################
#' WIDETABLE: a disk-backed table with (virtually) unlimited columns
#'
#' A \code{WIDETABLE} is a reference class that stores every column of a table in its
#' own file inside the folder \code{<path>/<name>}, together with a small metadata file
#' (\code{.meta.rds}). Columns are read from disk only when they are needed, and a
#' size-limited in-memory cache (field \code{data}) keeps the most recently used
#' columns, so the number of columns is limited only by the storage space.
#'
#' A \code{WIDETABLE} can be used much like a \code{data.frame}:
#' \code{wt[rows, cols]}, \code{wt[cols]}, \code{wt[[col]]}, \code{wt[rows, cols] <- value},
#' \code{wt[[col]] <- vector}, \code{nrow}, \code{ncol}, \code{dim}, \code{colnames},
#' \code{head}, \code{tail}, \code{rbind}, \code{cbind}, \code{as.data.frame},
#' \code{as.matrix}, and \code{dplyr::select}/\code{dplyr::pull}.
#' Subsetting returns a \code{data.frame} (or a vector) when the estimated size of the
#' result fits in \code{size_limit}, otherwise a new \code{WIDETABLE} that refers to a
#' subset of rows and columns of the same files.
#'
#' Because \code{WIDETABLE} is a reference class, modifying methods (adding columns,
#' replacing values, binding rows) change the object in place and update the files on
#' disk immediately. There is no separate "save" step.
#'
#' @section Creating and opening tables:
#' \code{WIDETABLE(name = 'mytable', path = 'some/folder')} opens the table stored in
#' \code{some/folder/mytable} if it exists, otherwise creates an empty table (the folder
#' is created when the first column is added). See also \code{\link{as.WIDETABLE}}.
#'
#' @field name (character) name of the table; also the name of the folder holding the
#'   column files. Default: a random name \code{'WTnnnnn'}.
#' @field path (character) parent folder of the table folder. Default \code{'.'}.
#' @field meta (data.frame) one row per column with \code{column}, \code{class},
#'   \code{n_unique} (number of distinct values) and \code{memsize} (bytes in memory).
#'   For a row subset, \code{n_unique} and \code{memsize} of columns that were not read
#'   are estimates.
#' @field data (data.frame) in-memory cache of loaded columns (rows in \code{row_index}).
#' @field numrows (integer) number of rows of the full table stored on disk.
#' @field numcols (integer) number of columns.
#' @field row_index (integer) the rows (of the full table) this object represents.
#'   For a full table it is \code{1:numrows}.
#' @field size_limit (numeric) maximum size in bytes of the in-memory cache, and the
#'   threshold above which subsetting returns a \code{WIDETABLE} instead of a
#'   \code{data.frame}. Default \code{1e9} (1 GB).
#' @field cell_size (numeric) largest observed size in bytes per cell.
#' @field format (character) file format of the column files: \code{'rds'} (default for
#'   new tables) or \code{'RData'}. For an existing table the stored format is used.
#'
#' @examples
#' folder = tempfile('wt_example_'); dir.create(folder)
#' wt = WIDETABLE(name = 'cars', path = folder)
#' wt$add_table(mtcars)
#' dim(wt)
#' wt[1:5, c('mpg', 'cyl')]
#' wt[['hp']][1:3]
#' wt[wt[['cyl']] == 4, 'mpg']
#' # re-open the same table later (fast: only the metadata file is read):
#' wt2 = WIDETABLE(name = 'cars', path = folder)
#' identical(as.data.frame(wt2), as.data.frame(wt))
#' unlink(folder, recursive = TRUE)
#' @seealso \code{\link{as.WIDETABLE}}, \code{\link{WIDETABLE-subset}},
#'   \code{\link{WIDETABLE-replace}}, \code{\link{WIDETABLE-bind}}
#' @export WIDETABLE
#' @exportClass WIDETABLE
WIDETABLE = setRefClass(
  'WIDETABLE',
  fields  = list(name = "character", path = "character", meta = 'data.frame', data = 'data.frame',
                 numrows = 'integer', numcols = 'integer', row_index = 'integer', size_limit = "numeric",
                 cell_size = 'numeric', format = 'character'),
  methods = list(
    initialize = function(...){
      "Creates a new WIDETABLE or opens an existing one.
      Arguments are field values, typically name, path, and optionally size_limit and
      format. If folder <path>/<name> exists, the table stored there is opened: its
      metadata file is read (columns are loaded lazily). A folder written by an older
      version of rwidetable/rbig (no metadata file) is scanned once and the metadata file is created."
      callSuper(...)
      if (is.empty(name)) name <<- paste0('WT', sample(10000:99999, 1))
      if (is.empty(path)) path <<- '.'
      if (is.empty(size_limit)) size_limit <<- 1e9
      if (is.empty(cell_size)) cell_size <<- 0
      assert(length(name) == 1 && length(path) == 1, "Fields 'name' and 'path' must be single strings!")
      assert(dir.exists(path), sprintf("Given path '%s' does not exist!", path))
      meta    <<- wt_empty_meta()
      numrows <<- 0L
      numcols <<- 0L
      if (dir.exists(table_folder())) open_folder()
      if (is.empty(format)) format <<- 'rds'
      assert(length(format) == 1 && format %in% c('rds', 'RData'), "Field 'format' must be 'rds' or 'RData'!")
      row_index <<- seq_len(numrows)
      data      <<- empty_df(numrows)
      invisible(.self)
    },

    table_folder = function(){
      "Returns the folder containing the column files: <path>/<name>."
      file.path(path, name)
    },

    column_file = function(cn){
      "Returns the path of the file storing column cn."
      file.path(table_folder(), paste0(cn, '.', format))
    },

    meta_file = function(){
      "Returns the path of the metadata file."
      file.path(table_folder(), '.meta.rds')
    },

    is_subset = function(){
      "TRUE if this object represents a subset (or a reordering) of the rows on disk."
      length(row_index) != numrows || !identical(row_index, seq_len(numrows))
    },

    check_full = function(action = 'This operation'){
      "Raises an error if the object is a row subset (used by methods that change the
      number of rows or create column files)."
      assert(!is_subset(), paste(action, "is not possible on a row subset of a WIDETABLE. Use the full table."))
    },

    open_folder = function(){
      "Reads the metadata of the table stored in table_folder() (internal)."
      if (file.exists(meta_file())){
        m = readRDS(meta_file())
        meta      <<- m$meta
        numrows   <<- as.integer(m$numrows)
        numcols   <<- nrow(m$meta)
        format    <<- m$format
        cell_size <<- max(cell_size, m$cell_size)
      } else {
        # Legacy table (no metadata file): scan the column files once.
        fls = list.files(table_folder(), pattern = '\\.(rds|RData)$')
        if (length(fls) == 0) return(invisible(NULL))
        ext = sub('.*\\.', '', fls)
        fmt = names(sort(table(ext), decreasing = TRUE))[1]
        format <<- fmt
        for (fn in fls[ext == fmt]){
          cn  = substr(fn, 1, nchar(fn) - nchar(fmt) - 1)
          vec = read_file(cn)
          if (numcols == 0) numrows <<- length(vec)
          assert(length(vec) == numrows, sprintf("Column file '%s' has %s elements but the table has %s rows!", fn, length(vec), numrows))
          set_meta(cn, vec)
        }
        row_index <<- seq_len(numrows)
        write_meta()
      }
      invisible(NULL)
    },

    write_meta = function(){
      "Writes the metadata file of the full table (internal)."
      if (is_subset()) return(invisible(NULL))
      if (!dir.exists(table_folder())) dir.create(table_folder(), recursive = TRUE)
      write_atomic(list(meta = meta, numrows = numrows, format = format, cell_size = cell_size,
                        version = '1.4.0'), meta_file(), 'rds')
    },

    write_atomic = function(obj, file, fmt){
      "Writes obj to file through a temporary file, so a failure never leaves a
      half-written column file (internal)."
      tmp = paste0(file, '.tmp')
      if (fmt == 'rds'){
        saveRDS(obj, tmp)
      } else {
        .fvec = obj
        save(.fvec, file = tmp)
      }
      if (!file.rename(tmp, file)){
        unlink(file)
        assert(file.rename(tmp, file), sprintf("Could not write file '%s'!", file))
      }
      invisible(TRUE)
    },

    read_file = function(cn){
      "Reads the full column cn (all rows on disk) from its file."
      fn = column_file(cn)
      assert(file.exists(fn), sprintf("File of column '%s' not found: %s", cn, fn))
      if (format == 'rds') return(readRDS(fn))
      env = new.env()
      nms = load(fn, envir = env)
      env[[if ('.fvec' %in% nms) '.fvec' else nms[1]]]
    },

    write_column = function(cn, vec){
      "Writes the full column vector vec (length numrows) to the file of column cn."
      assert(length(vec) == numrows || numcols == 0, sprintf("Column '%s' must have %s elements!", cn, numrows))
      if (!dir.exists(table_folder())) dir.create(table_folder(), recursive = TRUE)
      write_atomic(vec, column_file(cn), format)
    },

    set_meta = function(cn, vec){
      "Adds or updates the meta row of column cn from vector vec (internal)."
      ms = as.numeric(utils::object.size(vec))
      nu = length(unique(vec))
      cl = class(vec)[1]
      i  = match(cn, meta$column)
      if (is.na(i)){
        meta[nrow(meta) + 1, ] <<- list(cn, cl, nu, ms)
        numcols <<- nrow(meta)
      } else {
        meta$class[i]    <<- cl
        meta$n_unique[i] <<- nu
        meta$memsize[i]  <<- ms
      }
      if (length(vec) > 0) cell_size <<- max(cell_size, ms / length(vec))
      invisible(NULL)
    },

    check_name = function(cn){
      "Checks that cn is a valid new column name (internal)."
      assert(is.character(cn) && length(cn) == 1 && !is.na(cn) && nchar(cn) > 0, 'Column name must be a non-empty string!')
      assert(!grepl('[/\\\\:*?"<>|]', cn) && !startsWith(cn, '.'),
             sprintf("Column name '%s' is not a valid file name (avoid / \\ : * ? \" < > | and a leading '.')!", cn))
      clash = meta$column[tolower(meta$column) == tolower(cn) & meta$column != cn]
      assert(length(clash) == 0, sprintf("Column name '%s' differs from existing column '%s' only by case; this is not portable across file systems!", cn, clash[1]))
    },

    save_column = function(cn){
      "Writes the cached column cn to its file (full tables only)."
      check_full('save_column()')
      assert(cn %in% names(data), sprintf("Column '%s' is not in memory!", cn))
      write_column(cn, data[[cn]])
      set_meta(cn, data[[cn]])
      write_meta()
    },

    add_column = function(vec, column_name = NULL, verbose = FALSE, save_meta = TRUE, ...){
      "Adds vector vec as a new column named column_name (default 'X<n>') and writes
      its file. The vector length must equal the number of rows (the first column added
      to an empty table sets the number of rows). A column whose name already exists is
      skipped with a warning (use wt[[name]] <- vec to replace it). Returns TRUE if the
      column was added, FALSE otherwise (invisibly)."
      check_full('Adding a column')
      if (inherits(vec, 'POSIXlt')) vec = as.POSIXct(vec)
      assert(is.atomic(vec) && is.null(dim(vec)), 'A column must be an atomic vector (not a list, matrix or data.frame)!')
      nn = length(vec)
      if (numcols == 0){
        numrows   <<- as.integer(nn)
        row_index <<- seq_len(nn)
        data      <<- empty_df(nn)
      } else {
        assert(nn == numrows, sprintf('Added vector must have exactly %s elements, but has %s!', numrows, nn))
      }
      colname = if (is.null(column_name)) paste0('X', numcols + 1) else as.character(column_name)
      if (colname %in% meta$column){
        warning(sprintf("Column '%s' already exists and was not added. Use wt[['%s']] <- value to replace it.", colname, colname), call. = FALSE)
        return(invisible(FALSE))
      }
      check_name(colname)
      write_column(colname, vec)
      set_meta(colname, vec)
      if (as.numeric(utils::object.size(vec)) <= size_limit){
        data[[colname]] <<- vec
        control_size(keep = colname)
      }
      if (save_meta) write_meta()
      if (verbose) message(colname, ' added!')
      invisible(TRUE)
    },

    add_table = function(df, verbose = FALSE, ...){
      "Adds every column of df (a data.frame, tibble or matrix) as a new column.
      Columns whose names already exist are skipped with a warning."
      if (inherits(df, 'WIDETABLE')){
        for (cn in df$meta$column) add_column(df$load_column(cn), column_name = cn, verbose = verbose, save_meta = FALSE)
        return(invisible(write_meta()))
      }
      if (is.matrix(df)) df = as.data.frame(df, stringsAsFactors = FALSE)
      assert(is.data.frame(df), 'Argument df must be a data.frame, tibble, matrix or WIDETABLE!')
      cns = colnames(df)
      if (is.null(cns)) cns = paste0('X', numcols + seq_len(ncol(df)))
      on.exit(write_meta())
      for (k in seq_along(cns)){
        add_column(df[[k]], column_name = cns[k], verbose = verbose, save_meta = FALSE)
      }
      invisible(TRUE)
    },

    remove_column = function(cn){
      "Deletes columns cn (character) from the table and from disk (full tables only)."
      check_full('Removing a column')
      for (col in cn){
        assert(col %in% meta$column, sprintf("Column '%s' does not exist!", col))
        unlink(column_file(col))
        meta <<- meta[meta$column != col, , drop = FALSE]
        rownames(meta) <<- NULL
        if (col %in% names(data)) data[[col]] <<- NULL
      }
      numcols <<- nrow(meta)
      if (numcols == 0){numrows <<- 0L; row_index <<- integer(0); data <<- empty_df(0)}
      write_meta()
    },

    rename_column = function(old, new){
      "Renames column old to new (renames its file)."
      check_full('Renaming a column')
      assert(old %in% meta$column, sprintf("Column '%s' does not exist!", old))
      assert(!new %in% meta$column, sprintf("Column '%s' already exists!", new))
      check_name(new)
      assert(file.rename(column_file(old), column_file(new)), 'Could not rename the column file!')
      meta$column[meta$column == old] <<- new
      if (old %in% names(data)) names(data)[names(data) == old] <<- new
      write_meta()
    },

    updated_copy = function(){
      "Returns a copy of this object (the copy refers to the same files)."
      copy()
    },

    fetch_column = function(cn){
      "Returns column cn (rows in row_index) from the cache or from disk, without
      adding it to the cache."
      if (cn %in% names(data)) return(data[[cn]])
      assert(cn %in% meta$column, sprintf("Column '%s' does not exist!", cn))
      vec = read_file(cn)
      if (is_subset()) vec = vec[row_index]
      vec
    },

    load_column = function(cn){
      "Returns column cn (rows in row_index). The column is read from disk if it is not
      in the cache, and then cached if it fits in size_limit (older cached columns are
      dropped as needed)."
      if (cn %in% names(data)) return(data[[cn]])
      vec = fetch_column(cn)
      if (as.numeric(utils::object.size(vec)) <= size_limit){
        data[[cn]] <<- vec
        control_size(keep = cn)
      }
      vec
    },

    load_columns = function(cols){
      "Returns columns cols (rows in row_index) as a data.frame. Cached columns are taken
      from memory, the others are read from disk (and not cached). The memory limit is not
      checked: make sure the result fits in memory."
      cols = as.character(cols)
      bad  = setdiff(cols, meta$column)
      assert(length(bad) == 0, paste('Unknown column(s):', paste(bad, collapse = ', ')))
      lst  = lapply(cols, function(cn) fetch_column(cn))
      names(lst) = cols
      list_to_df(lst, length(row_index))
    },

    control_size = function(keep = character()){
      "Drops cached columns, oldest first (except those in keep), until the cache is below
      size_limit."
      cached = names(data)
      if (length(cached) == 0) return(invisible(NULL))
      sizes = meta$memsize[match(cached, meta$column)]
      sizes[is.na(sizes)] = 0
      total = sum(sizes)
      if (total <= size_limit) return(invisible(NULL))
      drop = character()
      for (k in seq_along(cached)){
        if (total <= size_limit) break
        if (cached[k] %in% keep) next
        drop  = c(drop, cached[k])
        total = total - sizes[k]
      }
      if (length(drop) > 0) data <<- data[setdiff(cached, drop)]
      invisible(NULL)
    },

    clear = function(){
      "Empties the in-memory cache."
      data <<- empty_df(length(row_index))
      invisible(gc())
    },

    refresh = function(){
      "Re-reads the metadata from disk (for example after another object referring to the
      same table added columns) and empties the cache. Full tables only."
      check_full('refresh()')
      meta <<- wt_empty_meta(); numrows <<- 0L; numcols <<- 0L
      if (dir.exists(table_folder())) open_folder()
      row_index <<- seq_len(numrows)
      data      <<- empty_df(numrows)
      invisible(.self)
    },

    set_value = function(rows = NULL, cols = NULL, value, ...){
      "Equivalent to x[rows, cols] <- value. rows (integer positions or logical) and cols
      (names, positions or logical) default to all rows and all columns. New column names
      create new columns (filled with NA outside rows). value is recycled if it has length
      one; a data.frame or list value provides one element per column. The change is
      written to the column files immediately; on a row subset only the selected rows of
      the full column are changed."
      rows = wt_rows(.self, rows)
      if (is.null(cols)) cols = meta$column
      if (!is.character(cols)) cols = wt_cols(.self, cols)
      if (is.matrix(value)) value = as.data.frame(value, stringsAsFactors = FALSE)
      target = if (is.null(rows)) row_index else row_index[rows]
      multi  = is.list(value)
      if (multi) assert(length(value) %in% c(1, length(cols)), 'Number of value columns must match the number of selected columns!')
      new_cols = setdiff(cols, meta$column)
      if (length(new_cols) > 0) for (cn in new_cols) check_name(cn)
      for (k in seq_along(cols)){
        cn  = cols[k]
        val = if (multi) value[[min(k, length(value))]] else value
        if (inherits(val, 'POSIXlt')) val = as.POSIXct(val)
        assert(length(val) %in% c(1, length(target)),
               sprintf('Replacement has %s elements but %s rows are selected!', length(val), length(target)))
        full = if (cn %in% meta$column) read_file(cn) else wt_na_like(val, numrows)
        if (is.factor(full) && !is.factor(val)){
          newlev = setdiff(unique(as.character(val[!is.na(val)])), levels(full))
          if (length(newlev) > 0) levels(full) = c(levels(full), newlev)
        }
        full[target] = val
        write_column(cn, full)
        update_disk_meta(cn, full)
        if (cn %in% names(data)) data[[cn]] <<- full[row_index]
      }
      invisible(.self)
    },

    update_disk_meta = function(cn, full){
      "Updates the metadata of column cn, given its full vector, in this object and in the
      metadata file (internal)."
      set_meta(cn, full)
      if (!is_subset()){
        write_meta()
      } else {
        m = if (file.exists(meta_file())) readRDS(meta_file()) else list(meta = wt_empty_meta(), numrows = numrows, format = format, cell_size = 0)
        i = match(cn, m$meta$column)
        row = data.frame(column = cn, class = class(full)[1], n_unique = length(unique(full)),
                         memsize = as.numeric(utils::object.size(full)), stringsAsFactors = FALSE)
        if (is.na(i)) m$meta = rbind(m$meta, row) else m$meta[i, ] = row
        write_atomic(m, meta_file(), 'rds')
        # the subset object keeps estimates for its own rows:
        j = match(cn, meta$column)
        meta$memsize[j]  <<- meta$memsize[j] * length(row_index) / max(1, numrows)
        meta$n_unique[j] <<- min(meta$n_unique[j], length(row_index))
      }
    },

    rbind_table = function(tbl, fill = FALSE){
      "Appends the rows of tbl (a data.frame, tibble, matrix or another WIDETABLE) to the
      table and updates the column files. Columns are matched by name. If fill is FALSE
      (default) both tables must have the same set of columns; if TRUE, missing values are
      filled with NA and new columns of tbl are added. Full tables only."
      check_full('Binding rows')
      if (is.matrix(tbl)) tbl = as.data.frame(tbl, stringsAsFactors = FALSE)
      if (inherits(tbl, 'WIDETABLE')){
        cols_new = tbl$meta$column
        n_new    = length(tbl$row_index)
        getcol   = function(cn) tbl$fetch_column(cn)
      } else {
        assert(is.data.frame(tbl), 'Argument tbl must be a data.frame, tibble, matrix or WIDETABLE!')
        cols_new = colnames(tbl)
        n_new    = nrow(tbl)
        getcol   = function(cn) tbl[[cn]]
      }
      if (numcols == 0) return(invisible(add_table(tbl)))
      if (n_new == 0) return(invisible(.self))
      cols_old = meta$column
      missing  = setdiff(cols_old, cols_new)
      extra    = setdiff(cols_new, cols_old)
      if (!fill){
        assert(length(missing) == 0 && length(extra) == 0,
               paste0('Columns of the two tables do not match! ',
                      if (length(missing) > 0) paste0('Missing: ', paste(utils::head(missing, 10), collapse = ', '), '. '),
                      if (length(extra) > 0) paste0('Extra: ', paste(utils::head(extra, 10), collapse = ', '), '. '),
                      'Use fill = TRUE to fill with NA.'))
      }
      for (cn in extra) check_name(cn)
      n_old = numrows
      for (cn in cols_old){
        full = read_file(cn)
        add  = if (cn %in% cols_new) getcol(cn) else wt_na_like(full, n_new)
        assert(length(add) == n_new, sprintf("Column '%s' of the new table has a wrong length!", cn))
        full = wt_combine(full, add)
        numrows <<- as.integer(length(full))
        write_column(cn, full)
        set_meta(cn, full)
        numrows <<- as.integer(n_old)
      }
      for (cn in extra){
        add  = getcol(cn)
        full = wt_combine(wt_na_like(add, n_old), add)
        numrows <<- as.integer(length(full))
        write_column(cn, full)
        set_meta(cn, full)
        numrows <<- as.integer(n_old)
      }
      numrows   <<- as.integer(n_old + n_new)
      row_index <<- seq_len(numrows)
      data      <<- empty_df(numrows)
      write_meta()
      invisible(.self)
    },

    rbind_dataframe = function(df, fill = FALSE){
      "Appends the rows of data.frame df to the table (see rbind_table)."
      rbind_table(df, fill = fill)
    },

    leftjoin_dataframe = function(df, by){
      "Left-joins data.frame df on the key column(s) by: the columns of df that are not in
      by are added to the table, aligned to the rows of the table. Row count and order are
      preserved. The key must identify the rows of df uniquely; rows of the table with no
      match get NA. Full tables only."
      check_full('Joining')
      df = as.data.frame(df, stringsAsFactors = FALSE)
      by = verify(by, 'character', domain = intersect(meta$column, colnames(df)), null_allowed = FALSE)
      key = function(tbl){
        if (length(by) == 1) return(tbl[[by]])
        do.call(paste, c(lapply(tbl[by], as.character), sep = '\r'))
      }
      kdf = key(df)
      assert(!anyDuplicated(kdf), "The 'by' columns must identify the rows of df uniquely!")
      idx = match(key(load_columns(by)), kdf)
      on.exit(write_meta())
      for (cn in setdiff(colnames(df), by)){
        add_column(df[[cn]][idx], column_name = cn, save_meta = FALSE)
      }
      invisible(.self)
    },

    show = function(){
      "Prints a summary of the table."
      cat(sprintf("WIDETABLE '%s': %s rows x %s columns\n", name, length(row_index), nrow(meta)))
      cat(sprintf("Folder: %s (format: %s)\n", table_folder(), format))
      if (is_subset()) cat(sprintf("Row subset of a table with %s rows\n", numrows))
      cached = names(data)
      mem    = sum(meta$memsize[match(cached, meta$column)], na.rm = TRUE)
      cat(sprintf("Cached columns: %s (%.1f MB of %.1f MB limit)\n", length(cached), mem / 2^20, size_limit / 2^20))
      if (nrow(meta) > 0){
        shown = utils::head(meta$column, 10)
        cat('Columns:', paste(shown, collapse = ', '), if (nrow(meta) > 10) sprintf('... (+%s more)', nrow(meta) - 10), '\n')
      }
      invisible(NULL)
    }
  ))

#' Resolve a row selector of a WIDETABLE to integer positions
#'
#' @param x a \code{WIDETABLE}.
#' @param i \code{NULL}, a logical vector (recycled) or positive or negative integer
#'   positions relative to \code{x}.
#' @return Integer positions, or \code{NULL} for all rows.
#' @keywords internal
wt_rows = function(x, i){
  if (is.null(i)) return(NULL)
  n = length(x$row_index)
  if (is.logical(i)){
    assert(length(i) %in% c(1, n), sprintf('Logical row index must have length %s!', n))
    return(which(rep_len(i, n)))
  }
  assert(is.numeric(i), 'Rows must be selected by position or by a logical vector (WIDETABLE has no row names)!')
  i = i[i != 0]
  assert(!anyNA(i), 'Row index contains NA!')
  if (length(i) > 0 && all(i < 0)) return(seq_len(n)[i])
  assert(all(i > 0), "Can't mix positive and negative row positions!")
  assert(all(i <= n), sprintf('Row position out of range: the table has %s rows!', n))
  as.integer(i)
}

#' Resolve a column selector of a WIDETABLE to column names
#'
#' @param x a \code{WIDETABLE}.
#' @param j a character, numeric (positive or negative) or logical column selector.
#' @param allow_new (logical) allow names of columns that do not exist yet?
#' @return Character vector of column names.
#' @keywords internal
wt_cols = function(x, j, allow_new = FALSE){
  cols = x$meta$column
  if (is.factor(j)) j = as.character(j)
  if (is.character(j)){
    bad = setdiff(j, cols)
    assert(allow_new || length(bad) == 0, paste('Unknown column(s):', paste(utils::head(bad, 10), collapse = ', ')))
    return(j)
  }
  if (is.logical(j)){
    assert(length(j) %in% c(1, length(cols)), sprintf('Logical column index must have length %s!', length(cols)))
    return(cols[rep_len(j, length(cols))])
  }
  assert(is.numeric(j), 'Columns must be selected by name, position or a logical vector!')
  j = j[j != 0]
  assert(!anyNA(j) && all(abs(j) <= length(cols)), 'Column position out of range!')
  cols[j]
}

# Generic Functions:

#' Dimensions and column names of a WIDETABLE
#'
#' \code{dim}, \code{dimnames}, \code{names} and \code{length} methods. Through them
#' \code{nrow}, \code{ncol}, \code{colnames} and \code{NROW}/\code{NCOL} also work.
#' \code{nrow} is the number of rows of the current row subset; \code{length} and
#' \code{ncol} are the number of columns (as for a \code{data.frame}).
#'
#' @param x a \code{WIDETABLE}.
#' @return \code{dim}: integer vector of length 2; \code{dimnames}: list of
#'   \code{NULL} (no row names) and the column names; \code{names}: character;
#'   \code{length}: integer.
#' @name WIDETABLE-dims
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' dim(wt); nrow(wt); ncol(wt); colnames(wt)
#' unlink(folder, recursive = TRUE)
NULL

#' @rdname WIDETABLE-dims
#' @export
setMethod("dim", "WIDETABLE", function(x) c(length(x$row_index), nrow(x$meta)))

#' @rdname WIDETABLE-dims
#' @export
setMethod("dimnames", "WIDETABLE", function(x) list(NULL, x$meta$column))

#' @rdname WIDETABLE-dims
#' @export
setMethod("names", "WIDETABLE", function(x) x$meta$column)

#' @rdname WIDETABLE-dims
#' @export
setMethod("length", "WIDETABLE", function(x) nrow(x$meta))

#' Convert a WIDETABLE to a data.frame or matrix
#'
#' Reads all columns (rows of the current row subset) into memory. Make sure the result
#' fits in memory.
#'
#' @param x a \code{WIDETABLE}.
#' @param ... ignored.
#' @return A \code{data.frame} or a \code{matrix}.
#' @name WIDETABLE-convert
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(iris, name = 'iris', path = folder)
#' str(as.data.frame(wt))
#' unlink(folder, recursive = TRUE)
NULL

#' @rdname WIDETABLE-convert
#' @export
as.data.frame.WIDETABLE = function(x, ...){
  x$load_columns(x$meta$column)
}

#' @rdname WIDETABLE-convert
#' @export
as.matrix.WIDETABLE = function(x, ...){
  as.matrix(x$load_columns(x$meta$column))
}

#' Convert a table to a WIDETABLE
#'
#' Creates a \code{WIDETABLE} in folder \code{<path>/<name>} and writes every column of
#' \code{x} to it.
#'
#' @param x a \code{data.frame}, tibble or matrix (a \code{WIDETABLE} is returned as is).
#' @param name (character) name of the table (and of its folder).
#' @param path (character) existing parent folder. Default \code{'.'}.
#' @param overwrite (logical) if the table folder already exists: \code{FALSE} (default)
#'   raises an error, \code{TRUE} deletes the existing table first.
#' @param ... further fields passed to \code{WIDETABLE()}, e.g. \code{size_limit} or
#'   \code{format}.
#' @return A \code{WIDETABLE}.
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' wt
#' unlink(folder, recursive = TRUE)
#' @export
as.WIDETABLE = function(x, name = NULL, path = '.', overwrite = FALSE, ...){
  if (inherits(x, 'WIDETABLE')) return(x)
  if (is.null(name)) name = paste0('WT', sample(10000:99999, 1))
  folder = file.path(path, name)
  if (dir.exists(folder) && length(list.files(folder, all.files = TRUE, no.. = TRUE)) > 0){
    assert(overwrite, sprintf("Folder '%s' already exists. Use overwrite = TRUE to replace it or open it with WIDETABLE(name, path).", folder))
    unlink(folder, recursive = TRUE)
  }
  wt = WIDETABLE(name = name, path = path, ...)
  wt$add_table(x)
  wt
}

#' Extract one column of a WIDETABLE
#'
#' @param x a \code{WIDETABLE}.
#' @param i (character or integer) name or position of the column.
#' @param ... ignored.
#' @return The column as a vector (rows of the current row subset).
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' wt[['mpg']]
#' unlink(folder, recursive = TRUE)
#' @export
'[[.WIDETABLE' = function(x, i, ...){
  cn = wt_cols(x, i)
  assert(length(cn) == 1, 'Exactly one column must be selected with [[ ]]!')
  x$load_column(cn)
}

#' Subset a WIDETABLE, returning a WIDETABLE
#'
#' Creates a new \code{WIDETABLE} object that refers to the same column files but to a
#' subset of rows and columns. No column data are read from disk (unless
#' \code{update_meta = TRUE}).
#'
#' @param obj a \code{WIDETABLE}.
#' @param rows (integer or logical) rows (relative to \code{obj}); \code{NULL} for all.
#' @param figures (character) column names; \code{NULL} for all.
#' @param update_meta (logical) if \code{TRUE}, columns not in memory are read to compute
#'   exact \code{n_unique} and \code{memsize} for the row subset; otherwise these are
#'   estimated proportionally.
#' @return A \code{WIDETABLE}.
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt  = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' sub = extract.widetable(wt, rows = 1:10, figures = c('mpg', 'hp'))
#' dim(sub)
#' unlink(folder, recursive = TRUE)
#' @export
extract.widetable = function(obj, rows = NULL, figures = NULL, update_meta = FALSE){
  figures = if (is.null(figures)) obj$meta$column else wt_cols(obj, figures)
  rows    = wt_rows(obj, rows)
  out     = obj$copy()
  out$meta = obj$meta[match(figures, obj$meta$column), , drop = FALSE]
  rownames(out$meta) = NULL
  out$numcols = nrow(out$meta)
  cached = intersect(figures, names(obj$data))
  if (is.null(rows)){
    out$data = obj$data[cached]
    return(out)
  }
  out$row_index = obj$row_index[rows]
  dat = obj$data[cached]
  dat = list_to_df(lapply(dat, function(v) v[rows]), length(rows))
  out$data = dat
  ratio = length(rows) / max(1, length(obj$row_index))
  for (i in seq_len(nrow(out$meta))){
    cn = out$meta$column[i]
    if (cn %in% cached || update_meta){
      vec = out$fetch_column(cn)
      out$meta$n_unique[i] = length(unique(vec))
      out$meta$memsize[i]  = as.numeric(utils::object.size(vec))
    } else {
      out$meta$n_unique[i] = min(out$meta$n_unique[i], length(rows))
      out$meta$memsize[i]  = out$meta$memsize[i] * ratio
    }
  }
  out
}

#' Read a subset of a WIDETABLE into a data.frame
#'
#' Cached columns are taken from memory; other columns are read from disk. Newly read
#' columns are added to the cache as long as they fit in the free cache space.
#'
#' @param obj a \code{WIDETABLE}.
#' @param rows (integer or logical) rows; \code{NULL} for all rows.
#' @param figures (character, integer or logical) columns; \code{NULL} for all columns.
#' @param drop (logical) if \code{TRUE} and a single column is selected, return a vector.
#' @return A \code{data.frame} (or a vector).
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' extract.dataframe(wt, rows = 1:3, figures = c('mpg', 'hp'))
#' unlink(folder, recursive = TRUE)
#' @export
extract.dataframe = function(obj, rows = NULL, figures = NULL, drop = TRUE){
  figures = if (is.null(figures)) obj$meta$column else wt_cols(obj, figures)
  rows    = wt_rows(obj, rows)
  cached  = names(obj$data)
  sizes   = obj$meta$memsize[match(cached, obj$meta$column)]
  free    = obj$size_limit - sum(sizes, na.rm = TRUE)
  lst     = vector('list', length(figures))
  newly   = list()
  for (k in seq_along(figures)){
    cn = figures[k]
    if (cn %in% cached){
      v = obj$data[[cn]]
    } else {
      v  = obj$fetch_column(cn)
      ms = obj$meta$memsize[match(cn, obj$meta$column)]
      if (!is.na(ms) && ms <= free && is.null(newly[[cn]])){
        newly[[cn]] = v
        free = free - ms
      }
    }
    lst[[k]] = if (is.null(rows)) v else v[rows]
  }
  if (length(newly) > 0){
    dat = obj$data
    for (cn in names(newly)) dat[[cn]] = newly[[cn]]
    obj$data = dat
  }
  if (drop && length(figures) == 1) return(lst[[1]])
  names(lst) = figures
  list_to_df(lst, if (is.null(rows)) length(obj$row_index) else length(rows))
}

#' Subset a WIDETABLE like a data.frame
#'
#' \code{wt[i, j]} selects rows \code{i} and columns \code{j}; \code{wt[j]} selects
#' columns. The result is a \code{data.frame} (or a vector with \code{drop = TRUE} and a
#' single column in \code{wt[i, j]} form) if its estimated size fits in
#' \code{size_limit}; otherwise it is a subset \code{WIDETABLE}
#' (see \code{\link{extract.widetable}}) referring to the same files.
#'
#' @param x a \code{WIDETABLE}.
#' @param i rows: positive or negative positions or a logical vector. In the one-argument
#'   form \code{wt[j]}, the columns.
#' @param j columns: names, positive or negative positions or a logical vector.
#' @param ... ignored.
#' @param drop (logical) if \code{TRUE} (default) and a single column is selected with
#'   \code{wt[i, j]}, return a vector.
#' @return A \code{data.frame}, a vector or a \code{WIDETABLE}.
#' @name WIDETABLE-subset
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' wt[1:3, c('mpg', 'hp')]
#' wt[c('mpg', 'hp')]           # all rows, two columns
#' wt[wt[['am']] == 1, 'mpg']   # a vector
#' wt[1:3, ]
#' unlink(folder, recursive = TRUE)
#' @export
'[.WIDETABLE' = function(x, i, j, ..., drop = TRUE){
  nA = nargs() - !missing(drop)
  if (nA < 3){
    # x[] or x[j]
    rows = NULL
    cols = if (missing(i)) x$meta$column else wt_cols(x, i)
    drop = FALSE
  } else {
    rows = if (missing(i)) NULL else wt_rows(x, i)
    cols = if (missing(j)) x$meta$column else wt_cols(x, j)
  }
  nr  = if (is.null(rows)) length(x$row_index) else length(rows)
  est = sum(x$meta$memsize[match(cols, x$meta$column)], na.rm = TRUE) * nr / max(1, length(x$row_index))
  if (est > x$size_limit) return(extract.widetable(x, rows, cols))
  extract.dataframe(x, rows, cols, drop = drop)
}

#' Replace values in a WIDETABLE
#'
#' \code{wt[i, j] <- value}, \code{wt[j] <- value} and \code{wt[[j]] <- value} change
#' values and write the affected column files immediately (see the \code{set_value}
#' method of \code{\link{WIDETABLE}}). Assigning to a column name that does not exist
#' creates a new column. \code{wt[[j]] <- NULL} deletes column \code{j}.
#'
#' @param x a \code{WIDETABLE}.
#' @param i rows (positions or logical); in the forms \code{wt[j] <- v} and
#'   \code{wt[[j]] <- v}, the column(s).
#' @param j columns (names, positions or logical).
#' @param value replacement value(s): a vector (recycled if of length one) or, for several
#'   columns, a \code{data.frame} or list with one element per column.
#' @return The modified \code{WIDETABLE} (the same object).
#' @name WIDETABLE-replace
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' wt[1:2, 'mpg'] <- 0
#' wt[['kpl']] <- wt[['mpg']] * 0.425   # new column
#' wt[['kpl']] <- NULL                  # delete it
#' unlink(folder, recursive = TRUE)
#' @export
'[<-.WIDETABLE' = function(x, i, j, value){
  if (nargs() < 4){
    rows = NULL
    cols = if (missing(i)) x$meta$column else wt_cols(x, i, allow_new = TRUE)
  } else {
    rows = if (missing(i)) NULL else wt_rows(x, i)
    cols = if (missing(j)) x$meta$column else wt_cols(x, j, allow_new = TRUE)
  }
  x$set_value(rows = rows, cols = cols, value = value)
  x
}

#' @rdname WIDETABLE-replace
#' @export
'[[<-.WIDETABLE' = function(x, i, value){
  cn = wt_cols(x, i, allow_new = TRUE)
  assert(length(cn) == 1, 'Exactly one column must be selected with [[ ]]!')
  if (is.null(value)){
    if (cn %in% x$meta$column) x$remove_column(cn)
  } else if (!(cn %in% x$meta$column) && !x$is_subset() && length(value) == length(x$row_index)){
    x$add_column(value, column_name = cn)
  } else {
    x$set_value(rows = NULL, cols = cn, value = value)
  }
  x
}

#' First or last rows of a WIDETABLE
#'
#' @param x a \code{WIDETABLE}.
#' @param n (integer) number of rows. Default 6.
#' @param ... ignored.
#' @return A \code{data.frame}.
#' @name WIDETABLE-head
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' head(wt, 3)
#' tail(wt, 2)
#' unlink(folder, recursive = TRUE)
NULL

#' @rdname WIDETABLE-head
#' @importFrom utils head tail
#' @export
head.WIDETABLE = function(x, n = 6L, ...){
  nr = length(x$row_index)
  extract.dataframe(x, rows = seq_len(min(max(n, 0), nr)), drop = FALSE)
}

#' @rdname WIDETABLE-head
#' @export
tail.WIDETABLE = function(x, n = 6L, ...){
  nr = length(x$row_index)
  m  = min(max(n, 0), nr)
  extract.dataframe(x, rows = seq_len(m) + nr - m, drop = FALSE)
}

#' Bind rows or columns to a WIDETABLE
#'
#' \code{rbind(wt, df1, df2, ...)} appends the rows of data.frames, tibbles, matrices or
#' other \code{WIDETABLE}s to \code{wt}; \code{cbind(wt, df1, ...)} adds their columns.
#' The first argument must be the \code{WIDETABLE}. Both modify \code{wt} in place (and
#' its files) and return it.
#'
#' @param ... a \code{WIDETABLE} followed by the tables to bind.
#' @param fill (logical, \code{rbind} only) if \code{TRUE}, tables with different columns
#'   can be bound: missing values are filled with \code{NA}. Default \code{FALSE}.
#' @param deparse.level ignored.
#' @return The (modified) \code{WIDETABLE}.
#' @name WIDETABLE-bind
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars[1:10, ], name = 'cars', path = folder)
#' rbind(wt, mtcars[11:20, ], mtcars[21:32, ])
#' cbind(wt, data.frame(id = 1:32))
#' dim(wt)
#' unlink(folder, recursive = TRUE)
NULL

#' @rdname WIDETABLE-bind
#' @export
rbind.WIDETABLE = function(..., fill = FALSE, deparse.level = 1){
  args = list(...)
  x    = args[[1]]
  assert(inherits(x, 'WIDETABLE'), 'The first argument of rbind() must be the WIDETABLE!')
  for (a in args[-1]) if (!is.null(a)) x$rbind_table(a, fill = fill)
  x
}

#' @rdname WIDETABLE-bind
#' @export
cbind.WIDETABLE = function(..., deparse.level = 1){
  args = list(...)
  x    = args[[1]]
  assert(inherits(x, 'WIDETABLE'), 'The first argument of cbind() must be the WIDETABLE!')
  for (a in args[-1]){
    if (is.null(a)) next
    if (is.atomic(a) && is.null(dim(a))) a = data.frame(a)
    x$add_table(a)
  }
  x
}

#' Zero-row prototype of a WIDETABLE (for tidyselect)
#' @param x a \code{WIDETABLE}.
#' @return A zero-row \code{data.frame} with the columns and classes of \code{x}.
#' @keywords internal
wt_prototype = function(x){
  proto = lapply(x$meta$class, function(cl){
    switch(cl,
           numeric = numeric(0), integer = integer(0), character = character(0),
           logical = logical(0), factor = factor(character(0)), Date = as.Date(character(0)),
           POSIXct = as.POSIXct(character(0)), complex = complex(0), raw = raw(0),
           integer64 = numeric(0), logical(0))
  })
  names(proto) = x$meta$column
  list_to_df(proto, 0)
}

#' Select or pull columns of a WIDETABLE with dplyr
#'
#' Methods for \code{dplyr::select} and \code{dplyr::pull}. \code{select} supports all
#' \pkg{tidyselect} helpers (\code{starts_with()}, \code{where(is.numeric)}, ...) and
#' returns the same result as \code{wt[cols]} (a \code{data.frame}, or a \code{WIDETABLE}
#' when the selection is too large for \code{size_limit}). Renaming is not supported.
#'
#' @param .data a \code{WIDETABLE}.
#' @param ... tidyselect column specifications.
#' @param var column to pull (name, position or tidyselect expression; default last).
#' @param name ignored.
#' @return \code{select}: a \code{data.frame} or \code{WIDETABLE}; \code{pull}: a vector.
#' @name WIDETABLE-dplyr
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(iris, name = 'iris', path = folder)
#' if (requireNamespace('dplyr', quietly = TRUE)) {
#'   print(head(dplyr::select(wt, dplyr::starts_with('Sepal'))))
#'   print(table(dplyr::pull(wt, Species)))
#' }
#' unlink(folder, recursive = TRUE)
NULL

#' @rdname WIDETABLE-dplyr
#' @exportS3Method dplyr::select
select.WIDETABLE = function(.data, ...){
  pos  = tidyselect::eval_select(rlang::expr(c(...)), data = wt_prototype(.data))
  cols = .data$meta$column[pos]
  assert(identical(names(pos), cols), 'Renaming columns is not supported by select() on a WIDETABLE; use wt$rename_column().')
  .data[cols]
}

#' @rdname WIDETABLE-dplyr
#' @exportS3Method dplyr::pull
pull.WIDETABLE = function(.data, var = -1, name = NULL, ...){
  cn = tidyselect::vars_pull(.data$meta$column, !!rlang::enquo(var))
  .data$load_column(cn)
}

#' Block sizes and blocks for correlation (internal)
#' @param wt a \code{WIDETABLE}.
#' @param block_size (integer) columns per block or \code{NULL} to derive it from
#'   \code{size_limit}.
#' @param n_jobs (integer) number of blocks held in memory at the same time / 2.
#' @return A list of character vectors (column blocks).
#' @keywords internal
wt_cor_blocks = function(wt, block_size = NULL, n_jobs = 1){
  cols = wt$meta$column[wt$meta$class %in% c('numeric', 'integer', 'logical')]
  assert(length(cols) > 0, 'The WIDETABLE has no numeric column!')
  if (is.null(block_size)){
    n = max(1, length(wt$row_index))
    block_size = floor(wt$size_limit / (2 * n * 8 * max(1, n_jobs)))
  }
  block_size = max(1, as.integer(block_size))
  split(cols, ceiling(seq_along(cols) / block_size))
}

#' Correlation matrix of a WIDETABLE
#'
#' Computes the correlation matrix of all numeric (numeric, integer, logical) columns of
#' a \code{WIDETABLE} block by block, so that only two blocks of columns are in memory at
#' a time. By default the block size is chosen so that two blocks fit in
#' \code{size_limit}. \code{cor.widetable.multicore} computes the blocks in parallel on a
#' local cluster (package \pkg{parallel}); each worker reads its columns directly from
#' the column files.
#'
#' @param wt a \code{WIDETABLE}.
#' @param block_size (integer) number of columns per block; \code{NULL} (default) to
#'   derive it from \code{size_limit}.
#' @param use passed to \code{\link[stats]{cor}}. Default \code{'everything'}.
#' @param verbose (logical) print progress? Default \code{FALSE}.
#' @param n_jobs (integer) number of parallel workers. Default 4.
#' @return A square correlation \code{matrix} with column names as dimnames.
#' @examples
#' folder = tempfile(); dir.create(folder)
#' wt = as.WIDETABLE(mtcars, name = 'cars', path = folder)
#' all.equal(cor.widetable(wt, block_size = 3), cor(mtcars))
#' unlink(folder, recursive = TRUE)
#' @export
cor.widetable = function(wt, block_size = NULL, use = 'everything', verbose = FALSE){
  blocks = wt_cor_blocks(wt, block_size)
  cols   = unlist(blocks, use.names = FALSE)
  out    = matrix(NA_real_, length(cols), length(cols), dimnames = list(cols, cols))
  nb     = length(blocks)
  for (i in seq_len(nb)){
    X = as.matrix(wt$load_columns(blocks[[i]]))
    out[blocks[[i]], blocks[[i]]] = stats::cor(X, use = use)
    for (j in seq_len(nb)[-seq_len(i)]){
      v = stats::cor(X, as.matrix(wt$load_columns(blocks[[j]])), use = use)
      out[blocks[[i]], blocks[[j]]] = v
      out[blocks[[j]], blocks[[i]]] = t(v)
    }
    if (verbose) message(sprintf('Block row %s of %s finished (%s columns per block).', i, nb, length(blocks[[1]])))
  }
  out
}

#' @rdname cor.widetable
#' @export
cor.widetable.multicore = function(wt, n_jobs = 4, block_size = NULL, use = 'everything', verbose = FALSE){
  assert(requireNamespace('parallel', quietly = TRUE), "Package 'parallel' is required!")
  blocks = wt_cor_blocks(wt, block_size, n_jobs = n_jobs)
  cols   = unlist(blocks, use.names = FALSE)
  nb     = length(blocks)
  pairs  = list()
  for (i in seq_len(nb)) for (j in i:nb) pairs[[length(pairs) + 1]] = c(i, j)
  files  = lapply(blocks, function(b) vapply(b, wt$column_file, character(1)))
  ri     = if (wt$is_subset()) wt$row_index else NULL
  worker = function(pair, files, fmt, ri, use){
    rd = function(f){
      if (fmt == 'rds') v = readRDS(f) else {e = new.env(); nm = load(f, envir = e); v = e[[if ('.fvec' %in% nm) '.fvec' else nm[1]]]}
      if (!is.null(ri)) v = v[ri]
      as.numeric(v)
    }
    rdm = function(ff){m = do.call(cbind, lapply(ff, rd)); colnames(m) = names(ff); m}
    X = rdm(files[[pair[1]]])
    if (pair[1] == pair[2]) return(stats::cor(X, use = use))
    stats::cor(X, rdm(files[[pair[2]]]), use = use)
  }
  environment(worker) = globalenv()
  cl = parallel::makeCluster(n_jobs)
  on.exit(parallel::stopCluster(cl))
  res = parallel::parLapply(cl, pairs, worker, files = files, fmt = wt$format, ri = ri, use = use)
  out = matrix(NA_real_, length(cols), length(cols), dimnames = list(cols, cols))
  for (k in seq_along(pairs)){
    v = res[[k]]
    out[rownames(v), colnames(v)] = v
    out[colnames(v), rownames(v)] = t(v)
  }
  if (verbose) message(sprintf('%s blocks computed on %s workers.', length(pairs), n_jobs))
  out
}
