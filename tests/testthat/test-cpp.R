context("C++ tests:")

test_that("Catch unit tests pass", {
  library(entropy)
  expect_cpp_tests_pass("FSelectorRcpp")
})
