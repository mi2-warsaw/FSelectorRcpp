library(RWeka)
library(FSelectorRcpp)

dt = lapply(1:5, function(x) data.frame(x = rnorm(10, mean = 10 * x), y = x))
dt = Reduce(rbind, dt)
dt[,2] = as.factor(dt[,2])

x = dt$x
y = dt$y

fs_discretize(x, y)

plot(x,y)


system.time(Discretize(y~x))
system.time(fs_discretize(x, y))

all(fs_discretize(x, y) + 1 == as.numeric(Discretize(y~x)[,1]))

microbenchmark::microbenchmark(Discretize(y~x), fs_discretize(x, y))

y = y[order(x)]
x = sort(x)


fs_cut_index(x,y)

plot(fs_discretize(x, y))
