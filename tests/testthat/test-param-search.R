library(FSelectorRcpp)

context("Param search")

test_that("Exhaustive search", {
  evaluator <- function(subset, data, dependent = names(iris)[5]) {
    library(rpart)
    k <- 5
    splits <- runif(nrow(data))
    results <- sapply(1:k, function(i) {
      test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
      train.idx <- !test.idx
      test <- data[test.idx, , drop = FALSE]   #nolint
      train <- data[train.idx, , drop = FALSE] #nolint
      tree <- rpart(to_formula(subset, dependent), train)
      error.rate <- sum(test[[dependent]] != predict(tree, test, type = "c")) /
        nrow(test)
      return(1 - error.rate)
    })
    return(mean(results))
  }

  iris <- iris[sample.int(75), ]

  fit1 <- feature_search(
    attributes = names(iris)[-5],
    fun = evaluator, data = iris,
    mode = "exhaustive")
  fit2 <- feature_search(
    attributes = names(iris)[-5],
    fun = evaluator, data = iris,
    mode = "exhaustive", parallel = FALSE)

  expect_error(f_search(
    attributes = character(),
    fun = evaluator, data = iris,
    mode = "exhaustive"))
})
