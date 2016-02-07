library(FSelectorRcpp)
library(entropy)
library(microbenchmark)


x = rnorm(1000,mean = 1000)
microbenchmark(entropy(x), fs_numeric_entropy(x))


x = as.integer(rnorm(1000, mean = 1000, sd = 5))
microbenchmark(entropy(table(x)), fs_entropy1d(x))

