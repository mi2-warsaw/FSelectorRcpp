library(dplyr)

# nocov start
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

  weka = as.numeric(RWeka::Discretize(z ~ x, dt)[,1])
  fs   = as.numeric(discretize(dt$x, dt$z))


  expect_equal(weka, fs)

  weka = RWeka::Discretize(z ~ x, dt)[,1]
  fs   = discretize(dt$x, dt$z)
  levels(weka)
  levels(fs)

}
)


test_that("Discretization - single NA (independent variable)",
{
  iris$Sepal.Length[3] = NA

  Weka = as.numeric(RWeka::Discretize(Species ~ Sepal.Length, data = iris)[,1])
  Weka = c(Weka[1:2],NA,tail(Weka,-2))

  fs   = as.numeric(discretize(iris$Sepal.Length, iris$Species))

  expect_equal(Weka, fs)

})

test_that("Discretization - not supported data type - throw error.",
{
  x = "a"
  y = "b"
  expect_error(discretize(x,y))
})

test_that("Discretization - formula returns data.frame.",
{
  expect_s3_class(discretize(Species ~ ., iris),"data.frame")
})

test_that("Discretization - expect warning when there is non numeric column in formula.",
{
  dt = cbind(iris,b = "a")
  expect_warning(discretize(Species ~ ., dt))
})

test_that("Discretization - not implemented for data.frame",
{
  dt = cbind(iris,b = "a")
  expect_error(discretize(dt))
})


# nocov end

test_that("Discretization - equalsize",
{
  dt = lapply(1:5, function(xx)
  {
    x = rnorm(1000, mean = 10 * xx)
    y = rnorm(1000, mean = 0.5 * xx)
    z = 10 * xx + 0.5 * sqrt(xx)
    data.frame(x,y,z)
  })

  dt = do.call(bind_rows, dt)

  xx = discretize(dt$x, dt$y, control = mdlControl())
  yy = discretize(dt$x, dt$y, control = equalsizeControl(k = 20))

  length(levels(xx))
  length(levels(yy))
})
