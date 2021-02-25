# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.4     13 October 2020    Initial Issue
# 1.0.5     13 October 2020    generic methods exported

################## WIDE TABLES: ##################
#' @export WIDETABLE
#' @exportClass WIDETABLE
WIDETABLE = setRefClass(
  'WIDETABLE', 
  fields  = list(name = "character", path = "character", meta = 'data.frame', data = 'data.frame', 
                 numrows = 'integer', numcols = 'integer', row_index = 'integer', size_limit = "numeric", 
                 cell_size = 'numeric', format = 'character'), 
  methods = list(
    initialize = function(...){
      callSuper(...)
      if(is.empty(name)){name <<- paste0('WT', sample(10000:99999, 1))}
      if(is.empty(size_limit)){size_limit <<- 10^9}
      if(is.empty(cell_size)){cell_size <<- 0}
      if(is.empty(format)){format <<- 'RData'}
      format <<- format %>% verify('character', domain = c('RData', 'rds'), lengths = 1, default = 'rds')
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
      assert(!is.null(data[[cn]]), 'Given column does not exist in memory!')
      .fvec = data[[cn]]
      columns_path = path %>% paste(name, sep = '/')
      if(!file.exists(columns_path)){dir.create(columns_path)}
      
      switch(format, 
             'rds'   = {saveRDS(.fvec, paste0(columns_path, '/', cn, '.rds'))},
             'RData' = {save(.fvec, file = paste0(columns_path, '/', cn, '.RData'))})
    },
    
    add_column = function(vec, column_name = NULL, save = T, verbose = T){
      "Adds a new column to the list of columns"
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
      for(cn in colnames(df)){
        add_column(df[[cn]], column_name = cn, save = save)
      }
    },
    
    # update_meta = function(){},
    
    load_column = function(cn){
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
    
    control_size = function(){
      while(object.size(data) > size_limit){
        data[[1]] <<- NULL
      }
    },
    
    clear = function(){
      data <<- list()
      gc()
    },
    
    # Revisit this function. Maybe use load_columns or '[WIDETABLE' functions
    rbind_dataframe = function(df){
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

rbind_widetables= function(x, ...){
  valid_types = c('data.frame', 'tibble', 'matrix', 'data.table', 'WIDETABLE')
  args = list(...)
  for(arg in args){assert(inherits(arg, valid_types, 'Invalid argument type!'))}
  
  for(arg in args){x = rbind.WIDETABLE(x, arg)}
  return(x)
}

rbind.WIDETABLE = function(x, y){
  if(inherits(y, 'WIDETABLE')){
    x %>% rbind_widetable_with_widetable(y)
  } else {
    x %>% rbind_widetable_with_dataframe(y)
  }
  # todo: treat matrix accordingly
}

# Generic Functions:

#' @export
setMethod("names", "WIDETABLE", function(x) x$meta$column %>% unique %>% as.character)

#' @export
setMethod("colnames", "WIDETABLE", function(x) x$meta$column %>% unique %>% as.character)

#' @export
setMethod("nrow", "WIDETABLE", function(x) length(x$row_index))

#' @export
setMethod("ncol", "WIDETABLE", function(x) x$meta$column %>% unique %>% length)

# setMethod("head", "WIDETABLE", function(x, ...) head(x$data, ...))
# setMethod("tail", "WIDETABLE", function(x, ...) tail(x$data, ...))
#' @export
setMethod("dim", "WIDETABLE", function(x) c(length(x$row_index), x$numcols))

# setMethod("colSums", "WIDETABLE", function(x) colSums(x$data))
# setMethod("rowSums", "WIDETABLE", function(x) rowSums(x$data))
setMethod("length", "WIDETABLE", function(x) x$meta$column %>% unique %>% length)
# setMethod("show", "WIDETABLE", function(object) show(object$data))

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

#' @export
setMethod("as.matrix", "WIDETABLE", function(x) {
  # return(as.data.frame(x) %>% as.matrix)
  # Test if this is faster:
  x$load_columns(colnames(x)) %>% as.matrix  
})

#' @export
'[[.WIDETABLE' = function(obj, figure = NULL){
  figure %>% verify('character', domain = colnames(obj), lengths = 1)
  return(obj$load_column(figure))
}

# update_meta will update values in the meta table like n_unique and memsize by reading all columns specified in figures
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
  out$meta %<>% filter(column %in% figures)
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
  else figures = figures %>% unique %>% as.character %>% verify('character', domain = obj$meta$column)
  
  objout = extract.widetable(obj, rows, figures)
  
  nrw = chif(is.null(rows), length(obj$row_index), length(obj$row_index[rows]))
  ncl = length(figures)
  
  if(obj$cell_size*nrw*ncl > obj$size_limit) {
    return(objout)
  } else {
    return(extract.dataframe(objout, drop = drop))
  }
}

#' @export
extract.dataframe = function(obj, rows = NULL, figures = NULL, drop = T){
  if(is.null(figures)){figures = unique(obj$meta$column)}
  obj$meta %>% filter(column %in% figures) %>% pull(column) %>% unique -> columns
  
  nrw = chif(is.null(rows), length(obj$row_index), length(obj$row_index[rows]))
  out = rep(0, nrw) %>% as.data.frame %>% {.[-1]}
  ####
  
  added_cols    = columns %-% names(obj$data)
  existing_cols = names(obj$data) %^% columns
  free_memory   = obj$size_limit - object.size(obj$data[existing_cols])
  
  obj$meta %>% filter(column %in% added_cols) %>% arrange(memsize) %>% 
    mutate(cumems = cumsum(memsize)) %>% 
    filter(cumems < free_memory) %>% pull(column) -> allowed
  
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
  if(drop & ncol(out) == 1){return(out[,1])} else {return(out)}
  
}

################ Extending dplyr functions to support WIDETABLE ######
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

#' data.matrix = function(x){
#'   if(inherits(x, 'WIDETABLE')){
#'     tables = x$load_columns(colnames())
#'     out    = NULL
#'     for(tn in tables){
#'       if(is.null(out)) {out = x$load_table(tn) %>% data.matrix}
#'       else {out %<>% cbind(x$load_table(tn) %>% data.matrix)}
#'       x$control_size()
#'     }
#'     return(out)
#'   } else {return(base::data.matrix(x))}  
#' }


