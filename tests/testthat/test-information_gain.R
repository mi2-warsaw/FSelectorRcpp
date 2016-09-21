# nocov start

library(dplyr)
library(FSelector)
test_that("Comparsion with FSelector",
{

  expect_equal(information.gain(Species ~ ., data = iris)$attr_importance,
  information_gain(formula = Species ~ ., data = iris)$importance)

  expect_equal(gain.ratio(Species ~ ., data = iris)$attr_importance,
               information_gain(formula = Species ~ ., data = iris, type = "gainratio")$importance)

  expect_equal(symmetrical.uncertainty(Species ~ ., data = iris)$attr_importance,
               information_gain(formula = Species ~ ., data = iris, type = "symuncert")$importance)
})

test_that("Test character table",
{
  set.seed(500)
  dt = lapply(1:50, function(xx)
  {
    x = rnorm(1000, mean = 10 * xx)
    y = rnorm(1000, mean = 0.5 * xx)
    z = 10 * xx + 0.5 * sqrt(xx)
    data.frame(x,y,z)
  })

  dt = Reduce(rbind, dt)
  dt$z = as.factor(as.integer(round(dt$z)))


  formula = z ~ .
  data = dt

  expect_lt(sum(information.gain(z~., data)[,1] - information_gain(formula = z~.,data = data)[,1]), 1e-10)
})


library(FSelectorRcpp)
library(FSelector)
library(testthat)
library(Matrix)


test_that("Sparse matrix - basics",
{


  species = iris$Species
  x = (as.matrix(iris[,1:4]))
  mode(x) = "integer"
  x = Matrix(x, sparse = TRUE)

  mode(iris$Sepal.Length) = "integer"
  mode(iris$Sepal.Width)  = "integer"
  mode(iris$Petal.Length) = "integer"
  mode(iris$Petal.Width)  = "integer"

  expect_equal(information_gain(x, species)$importance,
               information_gain(formula = Species ~ ., data = iris)$importance)

  expect_equal(information_gain(x, species, type = "gainratio")$importance,
               information_gain(formula = Species ~ ., data = iris, type = "gainratio")$importance)

  expect_equal(information_gain(x, species, type = "symuncert")$importance,
               information_gain(formula = Species ~ ., data = iris, type = "symuncert")$importance)
})

test_that("Removing NAs in formula (order)",
{
  xx = data_frame(x = as.character(c(1,2,3)), y = as.character(c(1,2,3)), na = c(NA, NA, 1))

  expect_equal(information_gain(xx[,"x", drop = FALSE], xx$y)$importance,
  information_gain(y ~ x, xx)$importance)
})

test_that("Removing NAs in formula",
{
  xx = data_frame(x = as.character(c(1,2,3)), y = as.character(c(1,2,3)), na = c(NA, NA, 1))

  expect_warning(information_gain(xx[,c("x","na")], xx$y))
  expect_warning(information_gain(y ~ ., data = xx))
})

test_that("Interfaces errors",
{
  expect_error(information_gain())
  x = 1
  y = 1
  expect_error(information_gain(formula = x,
                                data = x,
                                x = x,
                                y = x))

  expect_error(information_gain(formula = x, data = x))

  xx = data_frame(x = as.character(c(1,2,3)), y = as.character(c(1,2,3)))
  expect_error(information_gain(x = y ~ ., y = xx))

})

# nocov end


