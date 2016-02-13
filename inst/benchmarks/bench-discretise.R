library(RWeka)
library(FSelectorRcpp)

dt = lapply(1:50, function(x) data.frame(x = rnorm(1e3), y = x))
dt = Reduce(rbind, dt)
dt[,2] = as.factor(dt[,2])

x = dt$x
y = dt$y



system.time(Discretize(y~x))
system.time(fs_discretize(x, y))

all(fs_discretize(x, y) + 1 == as.numeric(Discretize(y~x)[,1]))

microbenchmark::microbenchmark(Discretize(y~x), fs_discretize(x, y))
