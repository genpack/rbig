# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.4     13 October 2020    Initial Issue
# 1.0.5     13 October 2020    generic methods exported
# 1.0.7     07 June 2021       Minor bug in extract.dataframe() fixed: changed order of columns in the output before fix

################## WIDE TABLES: ##################
#' WIDETABLE: a disk-backed table with (virtually) unlimited columns
#'
#' A \code{WIDETABLE} is a reference class that stores every column of a table in a
#' separate file inside the folder \code{<path>/<name>}. Only a subset of columns is
#' kept in memory (field \code{data}); the total size of the cached columns is kept
#' below \code{size_limit} bytes. Columns are read from disk on demand, so the number
#' of columns is limited only by the storage space.
#'
#' A \code{WIDETABLE} can be used much like a \code{data.frame}: \code{wt[rows, cols]},
#' \code{wt[[col]]}, \code{nrow}, \code{ncol}, \code{dim}, \code{colnames},
#' \code{as.data.frame} and \code{as.matrix} are supported. Subsetting returns a
#' \code{data.frame} when the result fits in \code{size_limit}, otherwise a new
#' (row/column subset) \code{WIDETABLE}.
#'
#' @field name (character) name of the table; also the name of the folder holding the
#'   column files. Default: a random name \code{'WTnnnnn'}.
#' @field path (character) parent folder of the table folder. Default \code{'.'}.
#' @field meta (data.frame) one row per column with \code{column}, \code{class},
#'   \code{n_unique} and \code{memsize} (bytes).
#' @field data (data.frame) in-memory cache of loaded columns.
#' @field numrows (integer) number of rows of the full table.
#' @field numcols (integer) number of columns.
#' @field row_index (integer) indices of the rows (of the full table) this object
#'   represents; a subset table refers to a subset of rows of the column files.
#' @field size_limit (numeric) maximum size (bytes) of the in-memory cache.
#'   Default \code{1e9}.
#' @field cell_size (numeric) largest observed size (bytes) per cell, used to estimate
#'   the memory needed for a subset.
#' @field format (character) file format of the column files: \code{'rds'} or
#'   \code{'RData'}.
#'
#' @examples
#' \dontrun{
#' wt = WIDETABLE(name = 'cars', path = tempdir())
#' wt$add_table(mtcars)
#' dim(wt)
#' wt[1:5, c('mpg', 'cyl')]
#' wt[['hp']]
#' # re-open the same table later:
#' wt2 = WIDETABLE(name = 'cars', path = tempdir())
#' }
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
      Arguments are field values (name, path, format, size_limit, ...). If folder
      <path>/<name> exists, all column files in it are read and registered."
      callSuper(...)
      if(is.empty(name)){name <<- paste0('WT', sample(10000:99999, 1))}
      if(is.empty(size_limit)){size_limit <<- 10^9}
      if(is.empty(cell_size)){cell_size <<- 0}
      if(is.empty(format)){format <<- 'RData'}
      format <<- verify(format, 'character', domain = c('RData', 'rds'), lengths = 1, default = 'rds')
      if(is.empty(numcols)){numcols <<- 0L}
      if(is.empty(numrows)){numrows <<- 0L}
      row_index <<- sequence(numrows)
      if(is.empty(path)){path <<- '.'}
      assert(file.exists(path), 'Given path does not exist!')
      columns_path = path %>% paste(name, sep = '/')
      
      if(file.exists(columns_path)){
        fls = columns_path %>% list.files
        
        # if(length(fls) > 100){
        #   library(doParallel)
        #   cl = makeCluster(4)
        #   registerDoParallel(cl)
        #   warnif(getDoParWorkers() < 4, 'Parallel run is not working. It may take too long!')
        #   
        #   out = foreach(fn = path %>% paste(name, fls, sep = '/'), .combine = c, .errorhandling = 'stop') %dopar% {
        #     if(format == 'RData'){
        #       load(file = fn)
        #       data.frame(column = fn) .fvec
        #     } else {readRDS(fn)} 
        #   }
        #   names(out) <- fls
        #   
        # } else {}
        
        if(format == 'RData'){
          for(fn in fls){
            load(file = path %>% paste(name, fn, sep = '/'))
            add_column(.fvec, column_name = fn %>% gsub(pattern = paste0('.', format), replacement = ''), save = F, verbose = F)
          }
        } else {
          for(fn in fls){
            path %>% paste(name, fn, sep = '/') %>% readRDS -> .fvec
            add_column(.fvec, column_name = fn %>% gsub(pattern = paste0('.', format), replacement = ''), save = F, verbose = F)
          }
        } 
      }
    },
    
    save_column = function(cn){
      "Writes in-memory column cn to its file <path>/<name>/<cn>.<format>."
      assert(!is.null(data[[cn]]), 'Given column does not exist in memory!')
      .fvec = data[[cn]]
      columns_path = path %>% paste(name, sep = '/')
      if(!file.exists(columns_path)){dir.create(columns_path)}
      
      switch(format, 
             'rds'   = {saveRDS(.fvec, paste0(columns_path, '/', cn, '.rds'))},
             'RData' = {save(.fvec, file = paste0(columns_path, '/', cn, '.RData'))})
    },
    
    add_column = function(vec, column_name = NULL, save = T, verbose = T){
      "Adds vector vec as a new column named column_name (default 'X<n>').
      The vector length must equal the number of rows (the first column sets it).
      Columns whose name already exists are ignored with a warning.
      If save is TRUE the column file is written. verbose prints a message."
      nn = length(vec)
      if(is.empty(numrows) | (numrows == 0)){numrows <<- nn; row_index <<- sequence(numrows); data <<- rep(0, nn) %>% as.data.frame %>% {.[-1]}} 
      else {assert(nn == numrows, paste('Added vector must have exactly', numrows, 'elements,', 'but', 'has', nn, '!'))}
      
      if(is.null(column_name)) {colname = paste0('X', numcols + 1)} else {colname = column_name}
      # (isDupCol: is duplicated column)
      isDupCol = colname %in% meta$column
      if(isDupCol){
        cat("\n", "Warning: Column '", colname, "' was found among existing columns and was ignored!")
      } else {
        numcols   <<- as.integer(numcols + 1)
        memsize = object.size(vec)
        meta[numcols, 'column']   <<- colname
        meta[numcols, 'class']    <<- class(vec)
        meta[numcols, 'n_unique'] <<- unique(vec) %>% length
        meta[numcols, 'memsize']  <<- memsize
        cell_size  <<- max(cell_size, memsize/nn)
        size_limit <<- max(size_limit, memsize)
        
        # if(object.size(data) + memsize < size_limit){
        #   data[[colname]] <<- vec
        # }
        data[[colname]] <<- vec
        
        if(save) save_column(colname)
        control_size()
      }
      if(verbose) cat('\n', colname, ' added!')
    },
    
    add_table = function(df, save = T){
      "Adds every column of data.frame df as a new column (see add_column)."
      for(cn in colnames(df)){
        add_column(df[[cn]], column_name = cn, save = save)
      }
    },
    
    # update_meta = function(){},
    
    updated_copy = function(){
      "Returns a new WIDETABLE object with the same fields (sharing the same files)."
      cp = new('WIDETABLE')
      cp$name <- name
      cp$meta <- meta
      cp$row_index <- row_index
      cp$cell_size <- cell_size
      cp$data <- data
      cp$format <- format
      cp$numcols <- numcols
      cp$path <- path
      cp$size_limit <- size_limit
      
      return(cp)
    },
    
    load_column = function(cn){
      "Returns column cn (rows in row_index), reading it from disk and caching it if
      it is not already in memory."
      if(is.null(data[[cn]])){
        switch(format, 
               'rds' = {paste0(path, '/', name, '/', cn, '.rds') %>% readRDS -> .fvec},
               'RData' = {load(file = paste0(path, '/', name, '/', cn, '.RData'))})
        .fvec[row_index] ->> data[[cn]]
      }
      control_size()
      return(data[[cn]])
    },
    
    # ## This function loads all rows of the given columns without considering size. So don't directly use it
    load_columns = function(cols){
      "Reads columns cols from disk (rows in row_index) and returns them as a
      data.frame. The memory limit is not checked and the cache is not used."
      out = rep(0, length(row_index)) %>% as.data.frame %>% {.[-1]}
      
      if(format == 'RData'){
        for(cn in cols){
          load(file = paste0(path, '/', name, '/', cn, '.RData'))
          out[[cn]] <- .fvec[row_index]
        }
      } else {
        for(cn in cols){
          .fvec <- paste0(path, '/', name, '/', cn, '.rds') %>% readRDS
          out[[cn]] <- .fvec[row_index]
        }
      }
      return(out)
    },  
    
    # This function, replaces selected rows and columns with a certain value
    # equivalent to: x[rows, cols] <- value
    # if argument save is TRUE, this functions changes the values and automatically saves the changes.
    # So changes will remain in the table.(Values are changed next time you load the widetable)
    set_value = function(rows = NULL, cols = NULL, value, save = T){
      "Equivalent to x[rows, cols] <- value. rows and cols default to all.
      cols may be names or indices. If save is TRUE, column files are updated."
      if(is.null(rows)){rows = sequence(length(row_index))}
      if(is.null(cols)){cols = meta$column}
      
      if(inherits(cols, 'numeric')) cols = as.integer(cols)
      if(inherits(cols, 'integer')) cols = meta$column[cols]
      
      for(cn in cols){
        v = load_column(cn)
        v[rows] <- value
        icn = which(meta$column == cn)
        meta$n_unique[icn] <<- unique(v) %>% length
        meta$memsize[icn]  <<- object.size(v)
        data[, cn] <<- v
        if(save){
          save_column(cn)
        }
        control_size()
        gc()
      }
    },
    
    control_size = function(){
      "Drops cached columns (oldest first) until the cache is below size_limit."
      while(object.size(data) > size_limit){
        data[[1]] <<- NULL
      }
    },
    
    clear = function(){
      "Empties the in-memory cache."
      data <<- list()
      gc()
    },
    
    # Revisit this function. Maybe use load_columns or '[WIDETABLE' functions
    rbind_dataframe = function(df){
      "Appends the rows of data.frame df (which must have the same columns) to the
      table and saves the column files."
      # For now:
      columns = colnames(df)
      assert(columns %==% meta$column, 'Columns of the two table do not match!')
      row_index <<- sequence(numrows)
      for(cn in columns){
        val = load_column(cn)
        val %<>% c(df[[cn]])
        data[[cn]] <<- val
        save_column(cn)
        control_size()
      }
      numrows   <<- numrows + nrow(df)
      row_index <<- sequence(numrows)
      gc()
    },
    
    # Revisit this function. 
    leftjoin_dataframe = function(df, by){
      "Left-joins data.frame df on columns by and adds its other columns to the table."
      verify(by, 'character', domain = meta$column %^% colnames(df))
      tbl = load_columns(by)
      tbl %<>% left_join(df, by = by)
      add_table(tbl)
    }
    
    # fill_na_with = function(value = 0, cols = unique(meta$column)){
    #   for(cn in cols){
    #     val = load_table(cn)
    #     wna = is.na(val) %>% which
    #     if(length(wna) > 0){
    #       val[wna] <- value
    #       data[[cn]] <<- val
    #       save_column(cn)
    #     }
    #     control_size()
    #   }
    #   gc()
    # }
  ))

#' Row-bind tables to a WIDETABLE (draft)
#'
#' Draft functions; they call helpers that do not exist yet.
#' @param x a WIDETABLE.
#' @param ... tables to bind.
#' @param y a table to bind.
#' @keywords internal
#' @noRd
rbind_widetables= function(x, ...){
  valid_types = c('data.frame', 'tibble', 'matrix', 'data.table', 'WIDETABLE')
  args = list(...)
  for(arg in args){assert(inherits(arg, valid_types, 'Invalid argument type!'))}
  
  for(arg in args){x = rbind.WIDETABLE(x, arg)}
  return(x)
}

#' @noRd
rbind.WIDETABLE = function(x, y){
  if(inherits(y, 'WIDETABLE')){
    x %>% rbind_widetable_with_widetable(y)
  } else {
    x %>% rbind_widetable_with_dataframe(y)
  }
  # todo: treat matrix accordingly
}

# Generic Functions:

#' Dimensions and column names of a WIDETABLE
#'
#' S4 methods that make a \code{WIDETABLE} behave like a \code{data.frame}.
#' \code{names} and \code{colnames} return the column names, \code{nrow} the number
#' of rows (of the current row subset), \code{ncol} and \code{length} the number of
#' columns and \code{dim} both.
#'
#' @param x a \code{WIDETABLE}.
#' @return \code{names}/\code{colnames}: character; \code{nrow}, \code{ncol},
#'   \code{length}: integer; \code{dim}: integer vector of length 2.
#' @name WIDETABLE-dims
#' @aliases names,WIDETABLE-method colnames,WIDETABLE-method nrow,WIDETABLE-method
#'   ncol,WIDETABLE-method dim,WIDETABLE-method length,WIDETABLE-method
#' @examples
#' \dontrun{
#' wt = WIDETABLE(name = 'cars', path = tempdir()); wt$add_table(mtcars)
#' dim(wt); colnames(wt)
#' }
NULL

#' @rdname WIDETABLE-dims
#' @export
setMethod("names", "WIDETABLE", function(x) x$meta$column %>% unique %>% as.character)

#' @rdname WIDETABLE-dims
#' @export
setMethod("colnames", "WIDETABLE", function(x) x$meta$column %>% unique %>% as.character)

#' @rdname WIDETABLE-dims
#' @export
setMethod("nrow", "WIDETABLE", function(x) length(x$row_index))

#' @rdname WIDETABLE-dims
#' @export
setMethod("ncol", "WIDETABLE", function(x) x$meta$column %>% unique %>% length)

# setMethod("head", "WIDETABLE", function(x, ...) head(x$data, ...))
# setMethod("tail", "WIDETABLE", function(x, ...) tail(x$data, ...))
#' @rdname WIDETABLE-dims
#' @export
setMethod("dim", "WIDETABLE", function(x) c(length(x$row_index), x$numcols))

# setMethod("colSums", "WIDETABLE", function(x) colSums(x$data))
# setMethod("rowSums", "WIDETABLE", function(x) rowSums(x$data))
#' @rdname WIDETABLE-dims
setMethod("length", "WIDETABLE", function(x) x$meta$column %>% unique %>% length)
# setMethod("show", "WIDETABLE", function(object) show(object$data))

#' Convert a WIDETABLE to a data.frame or matrix
#'
#' Reads all columns (rows of the current row subset) into memory. Make sure the
#' result fits in memory.
#'
#' @param x a \code{WIDETABLE}.
#' @return A \code{data.frame} or a \code{matrix}.
#' @name WIDETABLE-convert
#' @aliases as.data.frame,WIDETABLE-method as.matrix,WIDETABLE-method
NULL

#' @rdname WIDETABLE-convert
#' @export
setMethod("as.data.frame", "WIDETABLE", function(x) {
  x$load_columns(colnames(x)) %>% as.data.frame
})

# setMethod("as.data.frame", "WIDETABLE", function(x) {
#   out    = NULL
#   for(cn in colnames(x)){
#     if(is.null(out)) {out = x$load_column(cn)}
#     else {out %<>% cbind(x$load_column(cn))}
#   }
#   out %<>% as.data.frame
#   colnames(out) <- colnames(x)
#   return(out)
# })

#' @rdname WIDETABLE-convert
#' @export
setMethod("as.matrix", "WIDETABLE", function(x) {
  # return(as.data.frame(x) %>% as.matrix)
  # Test if this is faster:
  x$load_columns(colnames(x)) %>% as.matrix  
})

#' Extract one column of a WIDETABLE
#'
#' @param obj a \code{WIDETABLE}.
#' @param figure (character) name of the column.
#' @return The column as a vector (rows of the current row subset).
#' @examples
#' \dontrun{
#' wt[['mpg']]
#' }
#' @export
'[[.WIDETABLE' = function(obj, figure = NULL){
  verify(figure, 'character', domain = colnames(obj), lengths = 1)
  return(obj$load_column(figure))
}

# update_meta will update values in the meta table like n_unique and memsize by reading all columns specified in figures
#' Subset a WIDETABLE, returning a WIDETABLE
#'
#' Creates a new \code{WIDETABLE} object that refers to the same column files but to
#' a subset of rows and columns. No column data are read from disk (unless
#' \code{update_meta = TRUE}).
#'
#' @param obj a \code{WIDETABLE}.
#' @param rows (integer) row indices (relative to \code{obj}); \code{NULL} for all rows.
#' @param figures (character) column names.
#' @param update_meta (logical) if \code{TRUE}, columns not in memory are read to compute
#'   exact \code{n_unique} and \code{memsize} for the row subset; otherwise they are
#'   estimated proportionally.
#' @return A \code{WIDETABLE}.
#' @export
extract.widetable = function(obj, rows, figures, update_meta = F){
  out = obj$copy()
  # todo (important): some columns in the meta table (like n_uniue) depend on the row subset
  # fix this issue asap, 
  # 1- If we have a subset of rows in the new table, values of each column in meta table should become NA for columns which are not read (Done)
  # 2- values of each column in meta table can be update here for columns in data (Done)
  
  # 3- meta update shall be embedded in load_column and load_columns() methods as well, 
  #    so that everytime the column is read, meta becomes updated!
  # 4- add, sum, mean, max, min, sd, var and some moments to the meta
  # 5- similar values for rows (a meta table for rows is required!)
  out$meta      <- data.frame(column = figures, stringsAsFactors = F) %>% 
    dplyr::left_join(obj$meta, by = 'column') %>% na.omit
  out$numcols   <- out$meta$column %>% unique %>% length
  if(is.null(rows)){
    out$row_index <- obj$row_index
    out$data      <- obj$data[, out$meta$column %^% names(obj$data), drop = F]
  } else {
    # removing n_unique causes error 
    # out$meta$n_unique = NA
    # removing memsize causes error in '[.WIDETABLE' method. 
    # out$meta$memsize  = NA
    out$row_index <- obj$row_index[rows]
    out$data      <- obj$data[rows, obj$meta$column %^% names(obj$data), drop = F]
    data_columns  <- colnames(out$data)

    rr = length(out$row_index)/out$numrows
    
    for(i in sequence(nrow(out$meta))){
      if(out$meta$column[i] %in% data_columns){
        vec = out$data[[out$meta$column[i]]]
        out$meta$n_unique[i] <- length(unique(vec))
        out$meta$memsize[i]  <- object.size(vec) 
      } else {
        if(update_meta){
          vec = out$load_columns(cn)[,1]
          out$meta$n_unique[i] <- length(unique(vec))
          out$meta$memsize[i]  <- object.size(vec) 
        } else {
          # estimates values
          out$meta$n_unique[i] <- ceiling(out$meta$n_unique[i]*rr)
          out$meta$memsize[i]  <- out$meta$memsize[i]*rr
        }
      }
    }
    out$numrows   <- length(out$row_index)
  }
  return(out)
}

#' Subset a WIDETABLE like a data.frame
#'
#' \code{wt[rows, cols]} returns a \code{data.frame} if the estimated size of the
#' result (\code{cell_size * rows * cols}) fits in \code{size_limit}, otherwise a
#' subset \code{WIDETABLE} (see \code{\link{extract.widetable}}).
#' \code{wt['col']} or \code{wt[c('a', 'b')]} selects columns.
#'
#' @param obj a \code{WIDETABLE}.
#' @param rows (integer or logical) rows to select, or a character vector of column
#'   names when \code{figures} is missing.
#' @param figures (character, integer or logical) columns to select.
#' @param drop (logical) if \code{TRUE} and a single column is selected, return a vector.
#' @return A \code{data.frame}, a vector or a \code{WIDETABLE}.
#' @examples
#' \dontrun{
#' wt[1:10, c('mpg', 'hp')]
#' wt[wt[['cyl']] == 4, ]
#' }
#' @export
'[.WIDETABLE'   = function(obj, rows = NULL, figures = NULL, drop = T){
  if(inherits(rows, 'logical')){rows = which(rows)}
  if(inherits(figures, 'logical')){figures = which(figures)}
  if(is.null(figures)){
    if(inherits(rows, 'character')){
      figures = rows
      rows  = NULL
    } else {
      figures = colnames(obj)
    }
  }
  if(inherits(figures, c('numeric', 'integer'))){figures = obj$meta$column[figures]} 
  else figures = verify(figures %>% unique %>% as.character, 'character', domain = obj$meta$column)
  
  objout = extract.widetable(obj, rows, figures)
  
  nrw = chif(is.null(rows), length(obj$row_index), length(obj$row_index[rows]))
  ncl = length(figures)
  
  if(obj$cell_size*nrw*ncl > obj$size_limit) {
    return(objout)
  } else {
    return(extract.dataframe(objout, drop = drop))
  }
}

#' Read a subset of a WIDETABLE into a data.frame
#'
#' Columns already cached are taken from memory; other columns are read from disk.
#' As many of the newly read columns as fit in the free cache space are kept in the
#' cache.
#'
#' @param obj a \code{WIDETABLE}.
#' @param rows (integer) row indices; \code{NULL} for all rows.
#' @param figures (character) column names; \code{NULL} for all columns.
#' @param drop (logical) if \code{TRUE} and a single column is selected, return a vector.
#' @return A \code{data.frame} (or a vector).
#' @export
extract.dataframe = function(obj, rows = NULL, figures = NULL, drop = T){
  if(is.null(figures)){figures = unique(obj$meta$column)}
  obj$meta$column %>% intersect(figures) -> columns
  
  nrw = chif(is.null(rows), length(obj$row_index), length(obj$row_index[rows]))
  out = rep(0, nrw) %>% as.data.frame %>% {.[-1]}
  ####
  
  added_cols    = columns %-% names(obj$data)
  existing_cols = names(obj$data) %^% columns
  free_memory   = obj$size_limit - object.size(obj$data[existing_cols])

  obj$meta[obj$meta$column %in% added_cols, ] %>% 
    dplyr::arrange(memsize) %>% 
    dplyr::mutate(cumems = cumsum(memsize)) %>% 
    dplyr::filter(cumems < free_memory) %>% pull(column) -> allowed
  
  obj$data[allowed] <- obj$load_columns(allowed)
  
  existing_cols = names(obj$data) %^% columns
  
  if(!is.null(rows)){
    out = obj$data[rows, existing_cols, drop = F]
  }
  else {
    out = obj$data[existing_cols]
  }
  
  ## data is filled. Do we have more columns?
  added_cols = columns %-% colnames(out)
  extra_data = obj$load_columns(added_cols)
  if(!is.null(rows)){extra_data = extra_data[rows, , drop = F]}
  
  out %<>%  cbind(extra_data)   
  
  obj$control_size()
  gc()
  if(drop & ncol(out) == 1){return(out[figures ,1])} else {return(out[figures])}
  
}

# Computes cross-correlation for widetables using multi-core
#' Correlation matrix of a WIDETABLE
#'
#' Computes the correlation matrix of all (numeric) columns of a \code{WIDETABLE}
#' block by block, so that only two blocks of columns are in memory at a time.
#' \code{cor.widetable.multicore} computes the blocks of each row of the block grid in
#' parallel with \pkg{doParallel}.
#'
#' @param wt a \code{WIDETABLE} with numeric columns.
#' @param n_jobs (integer) number of parallel workers.
#' @return A square correlation \code{matrix} with column names as dimnames.
#' @export
cor.widetable.multicore = function(wt, n_jobs = 4){
  # todo
  # Determine number of partitions:
  columns = colnames(wt)
  ncolwt  = length(columns)
  np_set  = ncolwt %>% rutils::divisors()
  is_df   = inherits(wt[columns], 'data.frame')
  i = 1
  while(!is_df){
    i = i + 1
    is_df   = inherits(wt[columns[sequence(ncolwt/np_set[i])]], 'data.frame')
  }
  # Run:
  library(doParallel)
  
  nn  = np_set[i]
  mm  = ncolwt/nn
  out = matrix(nrow = ncolwt, ncol = ncolwt, dimnames = list(columns, columns))
  
  for(i in sequence(nn)){
    X  = wt[columns[(i - 1)*mm + sequence(mm)]]
    cl = makeCluster(n_jobs)
    registerDoParallel(cl)
    warnif(getDoParWorkers() < n_jobs, 'Parallel run is not working. It may take too long!')
    jl = foreach(j = i:nn, .combine = c, .packages = c('magrittr', 'stats', 'dplyr', 'rutils', 'rbig'), .errorhandling = 'pass') %dopar% {
      if(j == i){
        v = cor(X)
      } else {
        Y = wt[columns[(j - 1)*mm + sequence(mm)]]
        v = cor(X, Y)
      }
      gc()
      list(v)
    }
    cat('\n', sprintf("Partition row %s Finished. Total grid size: %s x %s. Num columns in each partition: %s", i, nn, nn, mm))
    for(v in jl){
      out[rownames(v), colnames(v)] <- v
      out[colnames(v), rownames(v)] <- t(v)
    }
    stopCluster(cl)
  }
  return(out)
}

# Computes cross-correlation for widetables
#' @rdname cor.widetable.multicore
#' @export
cor.widetable = function(wt){
  # Determine number of partitions:
  columns = colnames(wt)
  ncolwt  = length(columns)
  np_set  = ncolwt %>% rutils::divisors()
  is_df   = inherits(wt[columns], 'data.frame')
  i = 1
  while(!is_df){
    i = i + 1
    is_df   = inherits(wt[columns[sequence(ncolwt/np_set[i])]], 'data.frame')
  }
  # Run:
  nn  = np_set[i]
  mm  = ncolwt/nn
  out = matrix(nrow = ncolwt, ncol = ncolwt, dimnames = list(columns, columns))
  
  for(i in sequence(nn)){
    X = wt[columns[(i - 1)*mm + sequence(mm)]]
    for(j in i:nn){
      if(j == i){
        v = cor(X)
      } else {
        Y = wt[columns[(j - 1)*mm + sequence(mm)]]
        v = cor(X, Y)
      }
      out[rownames(v), colnames(v)] <- v
      out[colnames(v), rownames(v)] <- t(v)
      cat('\n', sprintf("Partition row %s, column %s Finished. Total grid size: %s x %s. Num columns in each partition: %s", i, j, nn, nn, mm))
    }
  }
  return(out)
}


#' Replace values in a WIDETABLE
#'
#' \code{wt[rows, cols] <- value} and \code{wt[[col]] <- value} change values and save
#' the affected column files (see the \code{set_value} method of \code{\link{WIDETABLE}}).
#'
#' @param obj a \code{WIDETABLE}.
#' @param value replacement value(s).
#' @param ... \code{rows} and \code{cols}, passed to \code{set_value}.
#' @return The modified \code{WIDETABLE}.
#' @name WIDETABLE-replace
#' @export
'[<-.WIDETABLE' = function(obj, value, ...){
  obj$set_value(value = value, ...)
  return(obj)
}

#' @rdname WIDETABLE-replace
#' @export
'[[<-.WIDETABLE' = function(obj, value, ...){
  obj$set_value(value = value, rows = NULL, ...)
  return(obj)
}

################ Extending dplyr functions to support WIDETABLE ######
#' Select, pull and column-bind with WIDETABLE support
#'
#' \code{select} and \code{pull} dispatch to \code{dplyr} for ordinary tables and work
#' on a \code{WIDETABLE} by column name. \code{cbind} on a \code{WIDETABLE} returns a
#' copy with the columns of the other tables added; otherwise it calls
#' \code{base::cbind}.
#'
#' @param x a \code{WIDETABLE} or an ordinary table.
#' @param ... column names (or tidyselect expressions) for \code{select}/\code{pull};
#'   tables to bind for \code{cbind}.
#' @return \code{select}: a table; \code{pull}: a vector; \code{cbind}: a table.
#' @export
select = function(x, ...){
  lst = try(list(...), silent = T)
  isformula = inherits(lst, 'try-error')
  if(isformula){vars = tidyselect::vars_select(colnames(x), ...)} else vars = c(...)  
  if(inherits(x, 'WIDETABLE')) {
    x[vars]
  } else {
    lst = try(as.list(...), silent = T)
    if(inherits(lst, 'try-error')) dplyr::select(x, ...) else dplyr::select_(...)
  }
}

#' @rdname select
#' @export
pull = function(x, ...){
  if(inherits(x, 'WIDETABLE')) {
    lst = try(list(...), silent = T)
    isformula = inherits(lst, 'try-error')
    if(isformula){vars = tidyselect::vars_select(colnames(x), ...)} else vars = c(...)  
    x[[vars]]
  }
  else dplyr::pull(x, ...)
}

#' @rdname select
#' @export
cbind = function(x, ...){
  if(inherits(x, 'WIDETABLE')){
    lst = list(...)
    out = x$copy()
    for(df in lst){
      out$add_table(df)
    }
    return(out)
  } else {return(base::cbind(x, ...))}
}

# data.matrix = function(x){
#   if(inherits(x, 'WIDETABLE')){
#     tables = x$load_columns(colnames())
#     out    = NULL
#     for(tn in tables){
#       if(is.null(out)) {out = x$load_table(tn) %>% data.matrix}
#       else {out %<>% cbind(x$load_table(tn) %>% data.matrix)}
#       x$control_size()
#     }
#     return(out)
#   } else {return(base::data.matrix(x))}  
# }
