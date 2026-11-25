# Fresh temporary folder, deleted when the calling test ends.
local_folder = function(env = parent.frame()) withr::local_tempdir("wt_test_", .local_envir = env)


synthetic_df = function(n = 50, seed = 1){
  set.seed(seed)
  data.frame(
    id    = seq_len(n),
    x     = rnorm(n),
    y     = runif(n),
    grp   = factor(sample(c('a', 'b', 'c'), n, replace = TRUE)),
    label = sample(letters, n, replace = TRUE),
    flag  = sample(c(TRUE, FALSE, NA), n, replace = TRUE),
    day   = as.Date('2024-01-01') + seq_len(n),
    time  = as.POSIXct('2024-01-01 10:00:00', tz = 'UTC') + seq_len(n) * 3600,
    stringsAsFactors = FALSE
  )
}
