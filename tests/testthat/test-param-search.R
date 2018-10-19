library(FSelectorRcpp)

context("Param search")

if (require("doParallel")) {

test_that("Exhaustive search", {

  skip_on_cran()

  library(doParallel)
  cl <- makeCluster(2)
  registerDoParallel(cl)

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

  fitGreedyForward <- feature_search(
    attributes = names(iris)[-5],
    fun = evaluator, data = iris,
    mode = "greedy", parallel = FALSE, type = "forward")

  fitGreedyBackward <- feature_search(
    attributes = names(iris)[-5],
    fun = evaluator, data = iris,
    mode = "greedy", parallel = FALSE, type = "backward")

  check_best <- function(fit) {
    best <- fit$best[-length(fit$best)]
    best <- names(best)[best == 1]

    list(
      best = evaluator(best, iris),
      fit = tail(as.numeric(unlist(fit1$best)), 1)
    )
  }

  f1 <- check_best(fit1)
  f2 <- check_best(fit2)
  fgf <- check_best(fitGreedyForward)
  fgb <- check_best(fitGreedyForward)

  expect_equal(f1$best, f1$fit)
  expect_equal(f2$best, f2$fit)
  expect_equal(fgb$best, fgb$fit)

  expect_error(feature_search(
    attributes = character(),
    fun = evaluator, data = iris,
    mode = "exhaustive"))

  stopCluster(cl)
  registerDoSEQ()
})

}

# get_children

test_that("get_children works as expected.", {
  x <- FSelectorRcpp:::get_children(c(1, 1, 0, 0), "forward")
  expF <- rbind(c(1, 1, 1, 0), c(1, 1, 0, 1))
  expect_equal(x, expF)

  x <- FSelectorRcpp:::get_children(c(1, 1, 0, 0), "backward")
  expB <- rbind(c(0, 1, 0, 0), c(1, 0, 0, 0))
  expect_equal(x, expB)

  x <- FSelectorRcpp:::get_children(c(1, 1, 0, 0), "both")
  expect_equal(x, rbind(expF, expB))
})
