

test_that("Cutoff - test",
{
  # Prepare datasets
  testDataFrame <- data.frame(x = rnorm(100000))
  rownames(testDataFrame) <- paste0("row_",  1:100000)


  # Testing C++ version:
  x <- cutOff_k(rownames(testDataFrame), testDataFrame$x, k = 1894)

  # Testing regular FSelector
  x2 <- FSelector::cutoff.k(testDataFrame, 1894)

  expect_equal(x, x2)
})
