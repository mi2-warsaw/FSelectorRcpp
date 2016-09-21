context("C++")

library(entropy)

test_that("Catch unit tests pass", {
    expect_cpp_tests_pass("FSelectorRcpp")
})
