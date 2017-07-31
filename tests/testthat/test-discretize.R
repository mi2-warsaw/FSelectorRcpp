library(dplyr)
library(FSelector)
library(FSelectorRcpp)

iris_plus <- setNames(iris, gsub(
  pattern = "\\.",
  replacement = "+",
  x = colnames(iris)
))

test_that("Data frame output", {
  expect_s3_class(discretize(Species ~ ., iris),
                  class = "data.frame")
  expect_s3_class(discretize(Species ~ Sepal.Length, iris),
                  class = "data.frame")
  expect_s3_class(discretize(iris$Sepal.Length, iris$Species),
                  class = "data.frame")
})

test_that("Discretization - basic", {
  dt <- lapply(1:5, function(xx) {
    x <- rnorm(1000, mean = 10 * xx)
    y <- rnorm(1000, mean = 0.5 * xx)
    z <- 10 * xx + 0.5 * sqrt(xx)
    data.frame(x, y, z)
  })

  dt <- do.call(bind_rows, dt)

  dt$z <- as.factor(as.integer(round(dt$z)))

  weka <- as.numeric(RWeka::Discretize(z ~ x, dt)[, 1])
  fs <- as.numeric(discretize(dt$x, dt$z)[[1]])

  expect_equal(weka, fs)

  weka <- RWeka::Discretize(z ~ x, dt)[, 1]
  fs <- discretize(dt$x, dt$z)[[1]]
  levels(weka)
  levels(fs)
})


test_that("Discretization - single NA (independent variable)", {
  iris$Sepal.Length[3] <- NA

  Weka <- as.numeric(RWeka::Discretize(Species ~ Sepal.Length,
                                       data = iris)[, 1])
  Weka <- c(Weka[1:2], NA, tail(Weka, -2))

  fs <- as.numeric(discretize(iris$Sepal.Length, iris$Species)[[2]])

  expect_equal(Weka, fs)
})

test_that("Discretization - not supported data type - throw error.", {
  x <- "a"
  y <- "b"
  expect_error(discretize(x, y))
})

test_that("Discretization - formula returns data.frame.", {
  expect_s3_class(discretize(Species ~ ., iris), "data.frame")
})

test_that("Discretization - expect warning when there is non numeric column in
          formula.", {
  dt <- cbind(iris, b = "a")
  expect_warning(discretize(Species ~ ., dt))
})

test_that("Discretization - not implemented for data.frame", {
  dt <- cbind(iris, b = "a")
  expect_error(discretize(dt))
})

test_that("Discretization - equalsize - ordered.", {
  x <- 1:6
  y <- 1:6

  d <- discretize(x, y, control = equalsizeControl(k = 2))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 1, 2, 2, 2))

  d <- discretize(x, y, control = equalsizeControl(k = 3))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 2, 2, 3, 3))

  d <- discretize(x, y, control = equalsizeControl(k = 4))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 2, 2, 3, 4))

  d <- discretize(x, y, control = equalsizeControl(k = 5))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 2, 3, 4, 5))
})

test_that("Discretization - equalsize - reverse order", {
  x <- 6:1
  y <- 1:6

  d <- discretize(x, y, control = equalsizeControl(k = 2))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 1, 2, 2, 2) %>% rev)

  d <- discretize(x, y, control = equalsizeControl(k = 3))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 2, 2, 3, 3) %>% rev)

  d <- discretize(x, y, control = equalsizeControl(k = 4))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 2, 2, 3, 4) %>% rev)

  d <- discretize(x, y, control = equalsizeControl(k = 5))[[2]]
  expect_equal(as.numeric(d), c(1, 1, 2, 3, 4, 5) %>% rev)
})

test_that("Discretization - equalsize - pseudo-random order", {
  x <- c(6, 4, 2, 3, 1, 5)
  y <- 1:6

  d <- discretize(x, y, control = equalsizeControl(k = 2))[[2]]
  expect_equal(as.numeric(d), c(2, 2, 1, 1, 1, 2))

  d <- discretize(x, y, control = equalsizeControl(k = 3))[[2]]
  expect_equal(as.numeric(d), c(3, 2, 1, 2, 1, 3))

  d <- discretize(x, y, control = equalsizeControl(k = 4))[[2]]
  expect_equal(as.numeric(d), c(4, 2, 1, 2, 1, 3))

  d <- discretize(x, y, control = equalsizeControl(k = 5))[[2]]
  expect_equal(as.numeric(d), c(5, 3, 1, 2, 1, 4))
})

test_that("Zero split points", {
  set.seed(1)

  x <- rep(0, 10)
  y <- rep(0, 10)

  expect_warning(discretize(x, y))
})

test_that("List interface inside function", {
  fnc <- function(xx) {
    discretize(list(xx$"Sepal+Length", xx[[2]],
                    xx[["Petal+Length"]]), xx$Species)

  }

  expect_equal(
    colnames(fnc(iris_plus)),
    c("Species", "Sepal+Length", "Sepal+Width", "Petal+Length"))
})

test_that("Interfaces", {
  expect_equal(
    colnames(discretize(iris$Sepal.Length, iris[["Species"]])),
    c("Species", "Sepal.Length")
  )

  expect_equal(
    colnames(discretize(iris$Sepal.Length, iris[[5]])),
    c("Species", "Sepal.Length")
  )

  expect_equal(
    colnames(discretize(list(iris$Sepal.Length, iris[[2]],
                             iris[["Petal.Length"]]), iris$Species)),
    colnames(discretize(Species ~ . - Petal.Width, iris))
  )

  expect_equal(
    colnames(discretize(list(iris_plus$"Sepal+Length", iris_plus[[2]],
                             iris_plus[["Petal+Length"]]), iris_plus$Species)),
    c("Species", "Sepal+Length", "Sepal+Width", "Petal+Length")
  )
})
