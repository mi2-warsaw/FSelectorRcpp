library(FSelectorRcpp)
library(FSelector)

test_that("Cutoff - test", {
  # Prepare datasets
  testDataFrame <- data.frame(x = paste0("row_", 1:100000),
                              y = rnorm(100000),
                              stringsAsFactors = FALSE)
  rownames(testDataFrame) <- testDataFrame[[1]]

  x <- cut_attrs(testDataFrame, k = 1894)

  # Testing regular FSelector
  x2 <- FSelector::cutoff.k(testDataFrame[,2, drop = FALSE], 1894)

  expect_equal(x, x2)
})

test_that("Cutoff - errors", {
  attr <- data.frame(attr = c("a", "b"))
  expect_error(cut_attrs(attr))

  attr <- data.frame(attr = c("a", "b"), importance = c("a", "b"))
  expect_error(cut_attrs(attr))
})

test_that("Cutoff - test", {
  x <- c(1, 2, 3)
  expect_error(cut_attrs(x))
})

test_that("Convert factor to character", {
  x <- information_gain(Species ~ ., iris)
  x$attributes <- as.factor(x$attributes)
  expect_equal(class(cut_attrs(x)), "character")
})

test_that("Warnings", {
  x <- information_gain(Species ~ ., iris)

  expect_warning(cut_attrs(x, 0.1))
  expect_warning(cut_attrs(x, 10))
})
