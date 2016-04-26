

test_that("Test entropy",
{
  x = c(10,11,21,50.0,11)
  expect_equal(entropy(table(x)), fs_entropy1d(x))
})

test_that("Test numeric entropy",
{
  x = c(10,11,21,50.0,11)
  expect_equal(entropy(x), fs_numeric_entropy(x))
})


