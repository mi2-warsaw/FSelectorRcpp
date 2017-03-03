context("C++")

library(entropy)

if(getRversion() > "3.4.0") {
test_that("Catch unit tests pass", {
    expect_cpp_tests_pass("FSelectorRcpp")
})
}
