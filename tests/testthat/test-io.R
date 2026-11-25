test_that("RDS2Columns splits row chunks in row order into a WIDETABLE folder", {
  f  = local_folder()
  df = synthetic_df(30)
  chunks = file.path(f, 'chunks'); dir.create(chunks)
  saveRDS(df[1:10, ],  file.path(chunks, 'part-0001.rds'))
  saveRDS(df[11:20, ], file.path(chunks, 'part-0002.rds'))
  saveRDS(df[21:30, ], file.path(chunks, 'part-0003.rds'))
  written = RDS2Columns(chunks, file.path(f, 'tbl'), buffer_size = 3)
  expect_setequal(written, colnames(df))
  wt = WIDETABLE(name = 'tbl', path = f)
  expect_equal(as.data.frame(wt)[colnames(df)], df, ignore_attr = 'row.names')
  # resuming skips existing columns
  expect_length(RDS2Columns(chunks, file.path(f, 'tbl')), 0)
})

test_that("RData2Columns writes WIDETABLE-compatible RData files", {
  f  = local_folder()
  df = synthetic_df(20)
  chunks = file.path(f, 'chunks'); dir.create(chunks)
  tbl = df[1:10, ];  save(tbl, file = file.path(chunks, 'a.RData'))
  tbl = df[11:20, ]; save(tbl, file = file.path(chunks, 'b.RData'))
  RData2Columns(chunks, file.path(f, 'tbl'), columns = c('id', 'x', 'grp'))
  wt = WIDETABLE(name = 'tbl', path = f)
  expect_equal(wt$format, 'RData')
  expect_equal(wt[['id']], df$id)
  expect_equal(wt[['grp']], df$grp)
  expect_equal(read_table_from_columns.RData(file.path(f, 'tbl'), columns = 'x', filter = list(id = list(max = 3))),
               data.frame(x = df$x[1:3]))
})

test_that("parquet tools round-trip through arrow", {
  skip_if_not_installed('arrow')
  f  = local_folder()
  df = synthetic_df(30)[c('id', 'x', 'label', 'day')]
  pq = file.path(f, 'pq'); dir.create(pq)
  arrow::write_parquet(df[1:15, ],  file.path(pq, 'part-00000.snappy.parquet'))
  arrow::write_parquet(df[16:30, ], file.path(pq, 'part-00001.snappy.parquet'))
  writeLines('', file.path(pq, '_SUCCESS'))
  expect_equal(parquet2DataFrame(pq), df, ignore_attr = TRUE)
  wt = parquet2WIDETABLE(pq, name = 'tbl', path = f, buffer_size = 2)
  expect_equal(as.data.frame(wt)[colnames(df)], df, ignore_attr = TRUE)
})
