library(dplyr)
library(FSelectorRcpp)
library(Matrix)

if (require("FSelector")) {

  test_that("Comparsion with FSelector", {
    expect_equal(
      information.gain(Species ~ ., data = iris)$attr_importance,
      information_gain(formula = Species ~ ., data = iris)$importance
    )

    expect_equal(gain.ratio(Species ~ ., data = iris)$attr_importance,
                 information_gain(formula = Species ~ ., data = iris,
                                  type = "gainratio")$importance)

    expect_equal(symmetrical.uncertainty(Species ~ .,
                                         data = iris)$attr_importance,
                 information_gain(formula = Species ~ ., data = iris,
                                  type = "symuncert")$importance)
  })

  test_that("Test character table", {
    set.seed(500)

    dt <- lapply(1:50, function(xx) {
      x <- rnorm(1000, mean = 10 * xx)
      y <- rnorm(1000, mean = 0.5 * xx)
      z <- 10 * xx + 0.5 * sqrt(xx)
      data.frame(x, y, z)
    })

    dt <- Reduce(rbind, dt)
    dt$z <- as.factor(as.integer(round(dt$z)))

    formula <- z ~ .
    data <- dt

    expect_lt(sum(information.gain(z ~ ., data)[, 1]
                  - information_gain(
                    formula = z ~ ., data = data)$importance),
              1e-10)
  })

  test_that("Equal bin discretization", {
    fs <- information.gain(formula = Sepal.Length ~ ., data = iris)
    fsrcpp <- information_gain(formula = Sepal.Length ~ ., data = iris,
                               equal = TRUE)

    expect_equal(fs$attr_importance, fsrcpp$importance)
  })
}


test_that("Sparse matrix - basics", {
  species <- iris$Species
  x <- as.matrix(iris[, 1:4])
  mode(x) <- "integer"
  x <- Matrix(x, sparse = TRUE)

  iris2 <- iris
  mode(iris2$Sepal.Length) <- "integer"
  mode(iris2$Sepal.Width) <- "integer"
  mode(iris2$Petal.Length) <- "integer"
  mode(iris2$Petal.Width) <- "integer"

  expect_equal(information_gain(x, species)$importance,
               information_gain(formula = Species ~ ., data = iris2)$importance)

  expect_equal(
    information_gain(x, species, discIntegers = FALSE)$importance,
    information_gain(
      formula = Species ~ ., data = iris2,
      discIntegers = FALSE
    )$importance
  )


  expect_equal(information_gain(x, species, type = "gainratio")$importance,
               information_gain(formula = Species ~ ., data = iris2,
                                type = "gainratio")$importance)

  expect_equal(information_gain(x, species, type = "symuncert")$importance,
               information_gain(formula = Species ~ ., data = iris2,
                                type = "symuncert")$importance)

  # When there's no column names just indexes will be used
  colnames(x) <- NULL
  expect_equal(
    information_gain(x, species, type = "symuncert")$attributes,
    1:4
  )

})

test_that("Removing NAs in formula (order)", {
  xx <- data_frame(x = as.character(c(1, 2, 3)), y = as.character(c(1, 2, 3)),
                   na = c(NA, NA, 1))

  expect_equal(information_gain(xx[, "x"], xx$y)$importance,
               information_gain(y ~ x, xx)$importance)
})


test_that("Interfaces errors", {
  expect_error(information_gain())
  x <- 1
  y <- 1
  expect_error(information_gain(formula = x, data = x, x = x, y = x))

  expect_error(information_gain(formula = x, data = x))

  xx <- data_frame(x = as.character(c(1, 2, 3)), y = as.character(c(1, 2, 3)))
  expect_error(information_gain(x = y ~ ., y = xx))


})
test_that("Incorrect interface parameter specification", {
  irisX <- iris[-5]
  y <- as.vector(iris$Species)
  expect_error(information_gain(x = irisX))
  expect_error(information_gain(formula = Species ~ .))
  expect_error(information_gain(data = iris))
  expect_error(information_gain(y = y))

  expect_error(information_gain(x = irisX, data = iris))
  expect_error(information_gain(y = y, data = iris))
  expect_error(information_gain(x = irisX, data = iris))
  expect_error(information_gain(x = irisX, formula = Species ~ .))
  expect_error(information_gain(y = y, formula = Species ~ .))

  expect_error(information_gain(y = y, x = irisX, data = iris))
  expect_error(information_gain(y = y, formula = Species ~ ., data = iris))
  expect_error(information_gain(x = irisX, formula = Species ~ ., data = iris))
  expect_error(information_gain(y = y, x = irisX, formula = Species ~ .))

  expect_error(information_gain(
    y = y, x = irisX, data = iris,
    formula = Species ~ .))
  expect_error(information_gain(formula = x ~ 1, data = 1:10))

})

test_that("Warning when y is numeric", {
  dt <- data_frame(x = rnorm(10), y = rnorm(10))
  z <- rnorm(10)

  expect_warning(information_gain(y ~ x, dt))
  expect_warning(information_gain(dt, z))
})

test_that("Compare interfaces - formula vs x,y", {

  expect_equal(
    information_gain(Species ~ ., iris),
    information_gain(x = iris[, -5], y = iris$Species)
  )

})

test_that("Information gain - integer column - discIntegers", {

  dt <- data_frame(
    y = iris$Species,
    x = as.integer(iris$Sepal.Length),
    z = as.numeric(as.integer(iris$Sepal.Length))
  )

  # discretize integer value
  result <- information_gain(y ~ ., dt, discIntegers = TRUE)
  expect_equal(length(unique(result$importance)), 1)

  set.seed(123)
  x <- as.integer(runif(1000, 1, 100))
  dt1 <- data.frame(
    y = as.integer(runif(1000, 1, 100)),
    x = x, # int
    z = as.numeric(x), # numeric
    fc = factor(x) # factor
  )

  # discretize integer
  r1 <- information_gain(y ~ ., dt1)
  expect_equal(r1[[2]][[1]], r1[[2]][[2]]) # int is equal to numeric

  # do not discretize integer column
  r2 <- information_gain(y ~ ., dt1, discIntegers = FALSE)
  expect_equal(r2[[2]][[1]], r2[[2]][[3]]) # int is equal to factor

})
