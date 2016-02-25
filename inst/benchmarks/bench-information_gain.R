library(FSelector)
library(FSelectorRcpp)

dt = lapply(1:500, function(xx)
{
  x = rnorm(1000, mean = 10 * xx)
  y = rnorm(1000, mean = 0.5 * xx)
  z = 10 * xx + 0.5 * sqrt(xx)
  data.frame(x,y,z)
})

dt = Reduce(rbind, dt)

dt$z = as.factor(as.integer(round(dt$z)))

information.gain(z ~ ., dt)
information_gain(z ~ ., dt)


all(as.numeric(Discretize(z ~ x, dt)[,1]) == fs_discretize(dt$x, dt$z) + 1)

system.time(Discretize(z ~ x, dt))
system.time(fs_discretize(dt$x, dt$z))

system.time(fs_table_numeric2d(dt$x, dt$z))

system.time(fs_count_levels(dt$z))

dt[,2] = as.factor(dt[,2])
