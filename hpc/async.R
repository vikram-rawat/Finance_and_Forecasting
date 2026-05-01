# library: ----------------------------------
source("dependencies.R")
# section: ----------------------------------

daemons(4)

df <- as.data.frame(matrix(rnorm(25e5), ncol = 5))
shared_df <- share(df)

boot <- \(i) colMeans(data[sample(nrow(data), replace = TRUE), ])

# Without mori — each daemon deserializes its own copy
system.time(
  map(1:8, in_parallel(\(i) boot(i), boot = boot, data = df))
)

# With mori — each daemon maps the same shared memory
system.time(
  map(1:8, in_parallel(\(i) boot(i), boot = boot, data = shared_df))
)

daemons(0)
