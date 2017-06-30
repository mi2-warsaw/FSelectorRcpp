library(FSelectorRcpp)
library(microbenchmark)

x <- round(rnorm(10000), 3)

microbenchmark(length(unique(x)), fs_count_levels(x))

x <- as.character(x)
microbenchmark(length(unique(x)), fs_count_levels(x))


microbenchmark(length(levels(factor(x))), fs_count_levels(x))
