library(FSelectorRcpp)
library(FSelector)

test_that("Cutoff - test", {
  # Prepare datasets
  testDataFrame <- data.frame(x = rnorm(100000))
  rownames(testDataFrame) <- paste0("row_", 1:100000)

  x <- cut_attrs(testDataFrame, k = 1894)

  # Testing regular FSelector
  x2 <- FSelector::cutoff.k(testDataFrame, 1894)

  expect_equal(x, x2)
})

test_that("Cutoff - test", {
  x <- c(1, 2, 3)
  expect_error(cut_attrs(x))
})
