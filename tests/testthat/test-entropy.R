library(FSelectorRcpp)
library(entropy)

test_that("Test character table",
{
  x = c(10,11,21,50.0,11)
  expect_equal(entropy(table(x)), fs_freq_entropy(x))
})

test_that("Test character table",
{
  x = c(10,11,21,50.0,11)
  expect_equal(entropy(x), fs_numeric_entropy(x))
})


