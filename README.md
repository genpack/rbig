# rwidetable

Disk-backed wide tables for R, with (virtually) unlimited columns.

`rwidetable` (formerly `rbig`) provides `WIDETABLE`, a reference class that stores
each column of a table in its own file. Only the columns you use are read, and a
size-limited cache keeps the most recently used ones in memory, so the number of
columns is limited only by disk space, while the table behaves much like a
`data.frame`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("genpack/rbig")
```

## Quick start

```r
library(rwidetable)

# convert a data.frame (one file per column in ./tables/cars)
dir.create("tables")
wt = as.WIDETABLE(mtcars, name = "cars", path = "tables")

# use it like a data.frame
dim(wt)
wt[1:5, c("mpg", "hp")]
wt[wt[["cyl"]] == 4, "mpg"]
wt[["kpl"]] <- wt[["mpg"]] * 0.425      # new column, saved immediately

# append batches of rows
rbind(wt, mtcars)

# open it again later: only a small metadata file is read
wt = WIDETABLE(name = "cars", path = "tables")
```

See the vignette for a complete guide: building a table and feeding it data,
converting data.frames, binding data frames incrementally, and working with a
`WIDETABLE` like a `data.frame`:

```r
vignette("widetable", package = "rwidetable")
```

## Features

* `WIDETABLE(name, path)` creates or opens a table; `as.WIDETABLE(df)` converts one.
* `wt[i, j]`, `wt[j]`, `wt[[j]]`, `head()`, `tail()`, `nrow()`, `ncol()`, `dim()`,
  `colnames()`, `as.data.frame()`, `as.matrix()`, `dplyr::select()`, `dplyr::pull()`.
* `wt[i, j] <- value`, `wt[[j]] <- vector`, `wt[[j]] <- NULL`.
* `rbind(wt, df, ..., fill = TRUE)`, `cbind(wt, df)`, `wt$leftjoin_dataframe(df, by)`.
* Selections too large for memory return a `WIDETABLE` subset instead of a
  `data.frame`.
* `parquet2WIDETABLE()`, `RDS2Columns()`, `RData2Columns()` build tables from files
  without loading them fully into memory.
* `cor.widetable()` / `cor.widetable.multicore()` compute correlation matrices block
  by block.
