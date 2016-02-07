library(FSelectorRcpp)
library(microbenchmark)

x = rnorm(1e3)
microbenchmark(table(x), fs_table1d(x))

xx = rnorm(1e3)
xx = as.character(xx)
microbenchmark(table(xx), fs_table1d(xx))

xx = as.integer(rnorm(1e3))
microbenchmark(table(xx), fs_table1d(xx))

