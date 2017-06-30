library(FSelector)
library(FSelectorRcpp)
library(dplyr)

dt <- lapply(1:500, function(xx) {
  x <- rnorm(1000, mean = 10 * xx)
  y <- rnorm(1000, mean = 0.5 * xx)
  z <- 10 * xx + 0.5 * sqrt(xx)
  data.frame(x, y, z)
})

dt <- do.call(bind_rows, dt)

dt$z <- as.factor(as.integer(round(dt$z)))

system.time(information.gain(z ~ ., dt))
system.time(information_gain(z ~ ., dt))
system.time(information_gain(z ~ ., dt, threads = 2))
