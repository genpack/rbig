test_that("a WIDETABLE can be built from a data.frame and read back", {
  f  = local_folder()
  df = synthetic_df()
  wt = as.WIDETABLE(df, name = 'tbl', path = f)
  expect_s4_class(wt, 'WIDETABLE')
  expect_equal(dim(wt), dim(df))
  expect_equal(nrow(wt), nrow(df))
  expect_equal(ncol(wt), ncol(df))
  expect_equal(colnames(wt), colnames(df))
  expect_equal(names(wt), names(df))
  expect_equal(as.data.frame(wt), df, ignore_attr = 'row.names')
  expect_true(file.exists(file.path(f, 'tbl', '.meta.rds')))
  expect_setequal(list.files(file.path(f, 'tbl')), paste0(colnames(df), '.rds'))
})

test_that("an existing table is reopened lazily from its metadata", {
  f  = local_folder()
  df = synthetic_df()
  as.WIDETABLE(df, name = 'tbl', path = f)
  wt = WIDETABLE(name = 'tbl', path = f)
  expect_equal(ncol(wt$data), 0)            # nothing loaded on opening
  expect_equal(dim(wt), dim(df))
  expect_equal(wt$meta$class[wt$meta$column == 'time'], 'POSIXct')
  expect_equal(wt[['time']], df$time)
  expect_equal(as.data.frame(wt), df, ignore_attr = 'row.names')
})

test_that("legacy folders without metadata (RData, '.fvec') are scanned once", {
  f = local_folder()
  dir.create(file.path(f, 'old'))
  df = synthetic_df(10)
  for (cn in colnames(df)){ .fvec = df[[cn]]; save(.fvec, file = file.path(f, 'old', paste0(cn, '.RData'))) }
  wt = WIDETABLE(name = 'old', path = f)
  expect_equal(wt$format, 'RData')
  expect_equal(dim(wt), dim(df))
  expect_setequal(colnames(wt), colnames(df))
  expect_equal(wt[['x']], df$x)
  expect_true(file.exists(file.path(f, 'old', '.meta.rds')))
})

test_that("subsetting works like a data.frame", {
  f  = local_folder()
  df = synthetic_df()
  wt = as.WIDETABLE(df, name = 'tbl', path = f)
  expect_equal(wt[1:5, c('x', 'grp')], df[1:5, c('x', 'grp')], ignore_attr = 'row.names')
  expect_equal(wt[c('x', 'y')], df[c('x', 'y')], ignore_attr = 'row.names')
  expect_equal(wt[, c('x', 'y')], df[, c('x', 'y')], ignore_attr = 'row.names')
  expect_equal(wt[, 'x'], df$x)
  expect_equal(wt[3, 'label'], df[3, 'label'])
  expect_equal(wt[1:3, ], df[1:3, ], ignore_attr = 'row.names')
  expect_equal(wt[-(1:45), 'id'], df$id[-(1:45)])
  expect_equal(wt[df$x > 0, 'id'], df$id[df$x > 0])
  expect_equal(wt[wt[['grp']] == 'a', c('id', 'grp'), drop = FALSE], df[df$grp == 'a', c('id', 'grp')], ignore_attr = 'row.names')
  expect_equal(wt[2:3, 'x', drop = FALSE], df[2:3, 'x', drop = FALSE], ignore_attr = 'row.names')
  expect_equal(wt[, 2], df[, 2])
  expect_equal(wt[[2]], df[[2]])
  expect_equal(wt[['day']], df$day)
  expect_equal(head(wt, 3), head(df, 3), ignore_attr = 'row.names')
  expect_equal(tail(wt, 2), tail(df, 2), ignore_attr = 'row.names')
  expect_error(wt[, 'nope'], 'Unknown column')
  expect_error(wt[1000, 'x'], 'out of range')
})

test_that("a large selection returns a WIDETABLE that refers to the same files", {
  f  = local_folder()
  df = synthetic_df(200)
  wt = as.WIDETABLE(df, name = 'tbl', path = f, size_limit = 2000)
  sub = wt[1:100, c('x', 'y', 'label')]
  expect_s4_class(sub, 'WIDETABLE')
  expect_equal(dim(sub), c(100L, 3L))
  expect_true(sub$is_subset())
  expect_equal(sub[['x']], df$x[1:100])
  expect_equal(sub[5:6, 'label'], df$label[5:6])
  sub2 = extract.widetable(sub, rows = 10:20)
  expect_equal(sub2[['y']], df$y[10:20])
})

test_that("the cache respects size_limit and load_column never returns NULL", {
  f  = local_folder()
  big = data.frame(a = runif(1000), b = runif(1000), c = runif(1000))
  wt = as.WIDETABLE(big, name = 'tbl', path = f, size_limit = 17000)
  wt$clear()
  for (cn in c('a', 'b', 'c')) expect_equal(wt$load_column(cn), big[[cn]])
  expect_lte(sum(vapply(wt$data, function(v) as.numeric(object.size(v)), 0)), 17000)
  expect_true('c' %in% names(wt$data))       # the last loaded column stays cached
  tiny = WIDETABLE(name = 'tbl', path = f, size_limit = 10)
  expect_equal(tiny$load_column('a'), big$a) # does not fit: returned but not cached
  expect_equal(ncol(tiny$data), 0)
})

test_that("replacing values on the full table and on a row subset updates the files correctly", {
  f  = local_folder()
  df = synthetic_df(20)
  wt = as.WIDETABLE(df, name = 'tbl', path = f)
  wt[1:2, 'x'] <- 0
  df$x[1:2] = 0
  expect_equal(wt[['x']], df$x)
  wt[['y']] <- 1
  df$y = 1
  expect_equal(WIDETABLE(name = 'tbl', path = f)[['y']], df$y)
  # row subset: only the selected rows of the full column change
  sub = extract.widetable(wt, rows = 11:20)
  sub[1:3, 'x'] <- 99
  df$x[11:13] = 99
  expect_equal(sub[['x']], df$x[11:20])
  re = WIDETABLE(name = 'tbl', path = f)
  expect_equal(re[['x']], df$x)
  expect_equal(nrow(re), 20)
  # several columns at once
  wt[1, c('x', 'y')] <- data.frame(x = -1, y = -2)
  expect_equal(wt[1, c('x', 'y')], data.frame(x = -1, y = -2))
  # factor columns accept new levels
  wt[1, 'grp'] <- 'zzz'
  expect_equal(as.character(wt[['grp']][1]), 'zzz')
  expect_error(wt[1:3, 'x'] <- 1:2, 'Replacement has 2 elements')
})

test_that("columns can be added, renamed and removed", {
  f  = local_folder()
  df = synthetic_df(10)
  wt = as.WIDETABLE(df, name = 'tbl', path = f)
  wt[['z']] <- wt[['x']] * 2
  expect_equal(wt[['z']], df$x * 2)
  wt[3:4, 'w'] <- 1                     # new column, NA elsewhere
  expect_equal(wt[['w']], c(NA, NA, 1, 1, rep(NA, 6)))
  expect_warning(wt$add_column(1:10, 'x'), 'already exists')
  expect_error(wt$add_column(1:3, 'k'), 'exactly 10 elements')
  expect_error(wt$add_column(1:10, 'a/b'), 'not a valid file name')
  expect_error(wt$add_column(1:10, 'X'), 'only by case')
  wt$rename_column('z', 'zz')
  expect_true('zz' %in% colnames(wt))
  wt[['zz']] <- NULL
  expect_false('zz' %in% colnames(wt))
  expect_false(file.exists(file.path(f, 'tbl', 'zz.rds')))
  re = WIDETABLE(name = 'tbl', path = f)
  expect_equal(colnames(re), colnames(wt))
  cbind(wt, data.frame(extra1 = letters[1:10], extra2 = 10:1))
  expect_equal(wt[['extra2']], 10:1)
  sub = extract.widetable(wt, rows = 1:5)
  expect_error(sub$add_column(1:5, 'nope'), 'row subset')
})

test_that("data frames can be bound incrementally", {
  f  = local_folder()
  df = synthetic_df(30)
  wt = WIDETABLE(name = 'tbl', path = f)
  for (part in split(df, rep(1:3, each = 10))) rbind(wt, part)
  expect_equal(dim(wt), dim(df))
  expect_equal(as.data.frame(wt), df, ignore_attr = 'row.names')
  expect_equal(levels(wt[['grp']]), levels(df$grp))
  expect_equal(attr(wt[['time']], 'tzone'), 'UTC')
  re = WIDETABLE(name = 'tbl', path = f)
  expect_equal(as.data.frame(re), df, ignore_attr = 'row.names')
  # mismatching columns
  expect_error(rbind(wt, df[1:2, 1:3]), 'do not match')
  rbind(wt, data.frame(id = 31L, new_col = 'n'), fill = TRUE)
  expect_equal(nrow(wt), 31)
  expect_true(is.na(wt[31, 'x']))
  expect_equal(wt[['new_col']], c(rep(NA, 30), 'n'))
  # another WIDETABLE
  wt2 = as.WIDETABLE(df[1:5, ], name = 'tbl2', path = f)
  rbind(wt, wt2, fill = TRUE)
  expect_equal(nrow(wt), 36)
  expect_equal(wt[32:36, 'id'], 1:5)
  # factor levels are merged
  rbind(wt, data.frame(id = 99L, grp = factor('new')), fill = TRUE)
  expect_true('new' %in% levels(wt[['grp']]))
})

test_that("left join keeps row count and order", {
  f  = local_folder()
  df = synthetic_df(10)
  wt = as.WIDETABLE(df[c('id', 'x')], name = 'tbl', path = f)
  lookup = data.frame(id = c(10L, 3L, 1L), score = c(100, 30, 10))
  wt$leftjoin_dataframe(lookup, by = 'id')
  expect_equal(nrow(wt), 10)
  expect_equal(colnames(wt), c('id', 'x', 'score'))
  expect_equal(wt[['score']], c(10, NA, 30, NA, NA, NA, NA, NA, NA, 100))
  expect_error(wt$leftjoin_dataframe(data.frame(id = c(1L, 1L), s2 = 1:2), by = 'id'), 'uniquely')
})

test_that("RData format works", {
  f  = local_folder()
  df = synthetic_df(10)
  wt = as.WIDETABLE(df, name = 'tbl', path = f, format = 'RData')
  expect_true(file.exists(file.path(f, 'tbl', 'x.RData')))
  re = WIDETABLE(name = 'tbl', path = f)
  expect_equal(re$format, 'RData')
  expect_equal(as.data.frame(re), df, ignore_attr = 'row.names')
  re[1, 'x'] <- 5
  expect_equal(WIDETABLE(name = 'tbl', path = f)[1, 'x'], 5)
})

test_that("as.WIDETABLE refuses to overwrite unless asked", {
  f = local_folder()
  as.WIDETABLE(mtcars, name = 'cars', path = f)
  expect_error(as.WIDETABLE(mtcars, name = 'cars', path = f), 'already exists')
  wt = as.WIDETABLE(iris, name = 'cars', path = f, overwrite = TRUE)
  expect_equal(colnames(wt), colnames(iris))
  expect_error(WIDETABLE(name = 'x', path = file.path(f, 'missing')), 'does not exist')
})

test_that("dplyr select and pull work", {
  skip_if_not_installed('dplyr')
  f  = local_folder()
  wt = as.WIDETABLE(iris, name = 'iris', path = f)
  expect_equal(dplyr::select(wt, dplyr::starts_with('Sepal')), iris[c('Sepal.Length', 'Sepal.Width')], ignore_attr = 'row.names')
  expect_equal(colnames(dplyr::select(wt, dplyr::where(is.factor))), 'Species')
  expect_equal(dplyr::pull(wt, Species), iris$Species)
  expect_equal(dplyr::pull(wt, 1), iris[[1]])
})

test_that("block-wise correlation matches stats::cor", {
  f  = local_folder()
  wt = as.WIDETABLE(mtcars, name = 'cars', path = f)
  expect_equal(cor.widetable(wt, block_size = 4), cor(mtcars))
  expect_equal(cor.widetable(wt), cor(mtcars))
  sub = extract.widetable(wt, rows = 1:20)
  expect_equal(cor.widetable(sub, block_size = 5), cor(mtcars[1:20, ]))
  skip_on_cran()
  expect_equal(cor.widetable.multicore(sub, n_jobs = 2, block_size = 5), cor(mtcars[1:20, ]))
})

test_that("as.matrix and show work", {
  f  = local_folder()
  wt = as.WIDETABLE(mtcars, name = 'cars', path = f)
  expect_equal(as.matrix(wt), as.matrix(mtcars), ignore_attr = TRUE)
  expect_output(print(wt), "WIDETABLE 'cars': 32 rows x 11 columns")
})
