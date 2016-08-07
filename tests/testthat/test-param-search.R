library(FSelectorRcpp)

context("Param search")

test_that("Exhaustive search",
{
  evaluator = function(subset, data)
  {
    library(rpart)
    k <- 5
    splits <- runif(nrow(data))
    results = sapply(1:k, function(i) {
      test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
      train.idx <- !test.idx
      test <- data[test.idx, , drop=FALSE]
      train <- data[train.idx, , drop=FALSE]
      tree <- rpart(to_formula(subset, "Species"), train)
      error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
      return(1 - error.rate)
    })
    return(mean(results))
  }


  iris = iris[sample.int(75),]

  fit1 = exhaustive_search(names(iris)[-5], evaluator, iris)
  fit2 = exhaustive_search(names(iris)[-5],
                                evaluator,
                                iris,
                                allowParallel = FALSE)

  expect_error(exhaustive_search(character(), evaluator, iris))
})

