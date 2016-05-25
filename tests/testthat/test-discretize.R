library(dplyr)
library(RWeka)

test_that("Discretization - basic",
{
  dt = lapply(1:5, function(xx)
  {
    x = rnorm(1000, mean = 10 * xx)
    y = rnorm(1000, mean = 0.5 * xx)
    z = 10 * xx + 0.5 * sqrt(xx)
    data.frame(x,y,z)
  })

  dt = do.call(bind_rows, dt)

  dt$z = as.factor(as.integer(round(dt$z)))

  weka = as.numeric(Discretize(z ~ x, dt)[,1])
  fs   = as.numeric(discretize(dt$x, dt$z))

  expect_equal(weka, fs)
}
)


test_that("Discretization - single NA (independent variable)",
{
  iris$Sepal.Length[3] = NA

  Weka = as.numeric(Discretize(Species ~ Sepal.Length, data = iris)[,1])
  Weka = c(Weka[1:2],NA,tail(Weka,-2))

  fs   = as.numeric(discretize(iris$Sepal.Length, iris$Species))

  expect_equal(Weka, fs)

})

