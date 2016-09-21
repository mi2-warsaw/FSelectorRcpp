library(FSelectorRcpp)
library(entropy)
library(microbenchmark)

x = as.integer(rnorm(1000, mean = 1000, sd = 5))
microbenchmark(entropy(table(x)), FSelectorRcpp:::fs_entropy1d(x))

