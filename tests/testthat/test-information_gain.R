library(dplyr)
library(FSelector)
test_that("Comparsion with FSelector",
{

  expect_equal(information.gain(Species ~ ., data = iris)$attr_importance,
  information_gain(Species ~ ., y = iris)$importance)

  expect_equal(gain.ratio(Species ~ ., data = iris)$attr_importance,
               information_gain(Species ~ ., y = iris, type = "gainratio")$importance)

  expect_equal(symmetrical.uncertainty(Species ~ ., data = iris)$attr_importance,
               information_gain(Species ~ ., y = iris, type = "symuncert")$importance)
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

  expect_lt(sum(information.gain(z~., data)[,1] - information_gain(z~., data)[,1]), 1e-10)
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
               information_gain(Species ~ ., y = iris)$importance)

  expect_equal(information_gain(x, species, type = "gainratio")$importance,
               information_gain(Species ~ ., y = iris, type = "gainratio")$importance)

  expect_equal(information_gain(x, species, type = "symuncert")$importance,
               information_gain(Species ~ ., y = iris, type = "symuncert")$importance)
})

