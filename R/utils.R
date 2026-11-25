# Header
# Filename:       utils.R
# Description:    Internal helper functions used across rwidetable.
#                 These used to come from package 'rutils'; they are now part of rwidetable
#                 so that the package is self-contained.

#' Is an object empty?
#'
#' \code{NULL}, zero-length vectors, empty lists, tables with no rows or no columns,
#' all-\code{NA} vectors and character vectors containing only \code{""} are empty.
#'
#' @param x any object.
#' @return \code{TRUE} or \code{FALSE}.
#' @keywords internal
is.empty = function(x){
  if (is.null(x)) return(TRUE)
  if (is.function(x) || is.environment(x)) return(FALSE)
  if (is.data.frame(x) || is.matrix(x)) return(any(dim(x) == 0))
  if (is.list(x)) return(length(x) == 0)
  if (length(x) == 0) return(TRUE)
  if (is.character(x)) return(all(is.na(x) | x == ''))
  all(is.na(x))
}

#' Stop with an error message unless a condition holds
#'
#' @param flag (logical) condition; if it has more than one element, all must be \code{TRUE}.
#' @param err_msg (character) error message.
#' @param ... ignored (kept for compatibility with the \pkg{rutils} version).
#' @return \code{NULL} invisibly; raises an error if \code{flag} is not all \code{TRUE}.
#' @keywords internal
assert = function(flag, err_msg = 'Assertion Error!', ...){
  if (length(flag) == 0 || anyNA(flag) || !all(flag)) stop(err_msg, call. = FALSE)
  invisible(NULL)
}

#' Print a warning message if a condition holds
#'
#' @param flag (logical) condition.
#' @param wrn_msg (character) message.
#' @param ... ignored.
#' @return \code{NULL} invisibly.
#' @keywords internal
warnif = function(flag, wrn_msg, ...){
  if (isTRUE(flag)) warning(wrn_msg, call. = FALSE)
  invisible(NULL)
}

#' Vectorless if-else
#'
#' @param a (logical) condition.
#' @param x value returned if \code{a} is \code{TRUE}.
#' @param y value returned if \code{a} is \code{FALSE}.
#' @return \code{x}, \code{y}, or \code{NULL} if \code{a} is empty.
#' @keywords internal
chif = function(a, x, y){
  if (is.empty(a)) return(NULL)
  if (isTRUE(a)) x else y
}

#' Verify an argument
#'
#' Checks the class, length, domain and element names of an argument and returns it
#' (or \code{default} if it is \code{NULL}).
#'
#' @param var the value to check.
#' @param allowed (character) allowed classes (checked with \code{inherits}).
#' @param domain allowed values. For character, factor and logical domains each element
#'   of \code{var} must be in \code{domain}; for numeric domains \code{var} must lie in
#'   \code{range(domain)}.
#' @param lengths (integer) allowed lengths.
#' @param null_allowed (logical) is \code{NULL} allowed? Default \code{TRUE}.
#' @param names_domain (character) allowed element names.
#' @param default value returned when \code{var} is \code{NULL}.
#' @param varname (character) name of the argument used in error messages.
#' @param ... ignored (kept for compatibility with the \pkg{rutils} version).
#' @return \code{var}, or \code{default} if \code{var} is \code{NULL}.
#' @keywords internal
verify = function(var, allowed = NULL, domain = NULL, lengths = NULL, null_allowed = TRUE,
                  names_domain = NULL, default = NULL, varname = deparse(substitute(var)), ...){
  force(varname)
  if (is.null(var)){
    if (null_allowed) return(default)
    stop(sprintf("NULL value is not allowed for argument '%s'!", varname), call. = FALSE)
  }
  if (!is.null(allowed) && !inherits(var, allowed)){
    stop(sprintf("Argument '%s' must be of class %s but is of class %s!", varname,
                 paste(allowed, collapse = ' or '), paste(class(var), collapse = '/')), call. = FALSE)
  }
  if (!is.null(lengths) && !(length(var) %in% lengths)){
    stop(sprintf("Argument '%s' must have length %s but has length %s!", varname,
                 paste(lengths, collapse = ' or '), length(var)), call. = FALSE)
  }
  if (!is.null(names_domain)){
    bad = setdiff(names(var), names_domain)
    if (length(bad) > 0){
      stop(sprintf("Argument '%s' has invalid element(s): %s", varname, paste(bad, collapse = ', ')), call. = FALSE)
    }
  }
  if (!is.null(domain)){
    if (is.numeric(domain) && is.numeric(var)){
      bad = var[!is.na(var) & (var < min(domain) | var > max(domain))]
    } else {
      bad = var[!(var %in% domain)]
    }
    if (length(bad) > 0){
      stop(sprintf("Argument '%s' must be in (%s); invalid value(s): %s", varname,
                   paste(utils::head(domain, 20), collapse = ', '), paste(utils::head(bad, 20), collapse = ', ')), call. = FALSE)
    }
  }
  var
}

#' Internal set and string operators
#'
#' \code{\%++\%} pastes without separator, \code{\%-\%} is \code{setdiff},
#' \code{\%^\%} is \code{intersect}, \code{\%<\%} tests whether \code{obj1} is a subset
#' of \code{obj2} and \code{\%==\%} tests set equality.
#'
#' @param obj1,obj2 vectors.
#' @return A vector (\code{\%++\%}, \code{\%-\%}, \code{\%^\%}) or a single logical.
#' @name set-operators
#' @keywords internal
NULL

#' @rdname set-operators
`%++%` = function(obj1, obj2) paste0(obj1, obj2)
#' @rdname set-operators
`%-%`  = function(obj1, obj2) setdiff(obj1, obj2)
#' @rdname set-operators
`%^%`  = function(obj1, obj2) intersect(obj1, obj2)
#' @rdname set-operators
`%<%`  = function(obj1, obj2) all(obj1 %in% obj2)
#' @rdname set-operators
`%==%` = function(obj1, obj2) (obj1 %<% obj2) && (obj2 %<% obj1)

#' Empty data.frame with a given number of rows
#'
#' @param n (integer) number of rows.
#' @return A \code{data.frame} with \code{n} rows and no columns.
#' @keywords internal
empty_df = function(n){
  structure(list(), names = character(0), class = 'data.frame', row.names = .set_row_names(as.integer(n)))
}

#' Build a data.frame from a named list of equal-length vectors (fast)
#'
#' Unlike \code{data.frame()}, column names are kept as they are and no
#' conversion is made.
#'
#' @param lst named list of vectors of equal length \code{n}.
#' @param n (integer) number of rows.
#' @return A \code{data.frame}.
#' @keywords internal
list_to_df = function(lst, n){
  structure(lst, names = names(lst), class = 'data.frame', row.names = .set_row_names(as.integer(n)))
}

#' Classes accepted as dates/times in filters
#' @keywords internal
#' @noRd
valid.time.classes = c('Date', 'POSIXct', 'POSIXlt', 'POSIXt', 'character', 'factor')

#' Convert a value to Date
#' @param x a character, Date or POSIX time.
#' @return A \code{Date}.
#' @keywords internal
char2Date = function(x){
  if (inherits(x, 'Date')) return(x)
  if (inherits(x, 'POSIXt')) return(as.Date(x))
  as.Date(as.character(x), tryFormats = c('%Y-%m-%d', '%Y/%m/%d', '%d/%m/%Y', '%d-%m-%Y'))
}

#' Convert a value to POSIXct
#' @param x a character, Date or POSIX time.
#' @return A \code{POSIXct}.
#' @keywords internal
char2Time = function(x){
  if (inherits(x, 'POSIXct')) return(x)
  if (inherits(x, c('POSIXlt', 'Date'))) return(as.POSIXct(x))
  as.POSIXct(as.character(x), tryFormats = c('%Y-%m-%d %H:%M:%OS', '%Y/%m/%d %H:%M:%OS',
                                             '%Y-%m-%d %H:%M', '%Y/%m/%d %H:%M', '%Y-%m-%d', '%Y/%m/%d'))
}
