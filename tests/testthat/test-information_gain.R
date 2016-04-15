library(FSelectorRcpp)
library(FSelector)
library(testthat)

test_that("Comparsion with FSelector",
{
  expect_equal(information.gain(Species ~ ., data = iris)$attr_importance,
  information_gain(Species ~ ., data = iris)$importance)

  expect_equal(gain.ratio(Species ~ ., data = iris)$attr_importance,
               information_gain(Species ~ ., data = iris, type = "gainratio")$importance)

  expect_equal(symmetrical.uncertainty(Species ~ ., data = iris)$attr_importance,
               information_gain(Species ~ ., data = iris, type = "symuncert")$importance)
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

  # Fropm FSelector:::information.gain.body
  # new_data = FSelector:::get.data.frame.from.formula(formula, data)
  # new_data = FSelector:::discretize.all(formula, new_data)
  # attr_entropies = sapply(new_data, FSelector:::entropyHelper)
  # class_entropy = attr_entropies[1]
  # attr_entropies = attr_entropies[-1]
  # joint_entropies = sapply(new_data[-1], function(t) {
  #   FSelector:::entropyHelper(data.frame(cbind(new_data[[1]], t)))
  # })
  #
  # formula = formula2names(formula,data)
  # values = information_gain_cpp(data[formula$x], data[[formula$y]])
  #
  # classDiff = abs(class_entropy - fs_entropy1d(data[[formula$y]]))
  # expect_less_than(classDiff, 10 * .Machine$double.eps)
  #
  # sum(attr_entropies - values$entropy)
  #
  # joint_entropies
  # values$joint

  expect_less_than(sum(information.gain(z~., data)[,1] - information_gain(z~., data)[,1]), 1e-10)
})



library(FSelectorRcpp)
library(FSelector)
library(testthat)

test_that("Sparse matrix - basics",
{
  species = iris$Species
  x = as.matrix(iris[,1:4])
  x = Matrix(x, sparse = TRUE)


  expect_equal(sp_information_gain(x, species)$importance,
               information_gain(Species ~ ., data = iris)$importance)

  expect_equal(sp_information_gain(x, species, type = "gainratio")$importance,
               information_gain(Species ~ ., data = iris, type = "gainratio")$importance)

  expect_equal(sp_information_gain(x, species, type = "symuncert")$importance,
               information_gain(Species ~ ., data = iris, type = "symuncert")$importance)
})

