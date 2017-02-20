utils::globalVariables("it")

#' @noRd
exhaustive_search <- function(attributes, fun, data,
                              subsetsSizes = 1:length(attributes),
                              allowParallel = TRUE, ...) {

  len <- length(attributes)
  fun <- match.fun(fun)
  zeroes <- rep(0, len)
  childComb <- NULL

  for (size in subsetsSizes) {
    childComb <- cbind(childComb,
                       apply(combn(1:len, size), 2, function(i) {
                         x <- zeroes
                         x[i] <- 1
                         x
                       }))
  }

  matIter <- iter(childComb, by = "column")

  if (allowParallel && getDoParRegistered()) {
    `%op%` <- `%dopar%`
    if (getDoParName() == "doSNOW") {
      tpb <- txtProgressBar(max = matIter$length, style = 3)
      progress <- function(n) {
        setTxtProgressBar(pb = tpb, value = n)
      }
      opts <- list(progress = progress)
    } else {
      opts <- NULL
    }
  } else {
    `%op%` <- `%do%`
    opts <- NULL
  }

  result <- foreach(it = matIter, .options.snow = opts, ...) %op% {
    fun(attributes[as.logical(as.numeric(it))], data)
  }

  maxIdx <- which.max(result)
  allResults <- setNames(as.data.frame(t(rbind(childComb, result))),
                         c(attributes, "values"))
  bestResult <- allResults[maxIdx, ]
  res <- list(bestResult = bestResult,
              allResults = allResults,
              fun = fun)

  return(res)
}

#' @noRd
greedy_search <- function(attributes, fun, data,
                          type = c("forward", "backward"),
                          allowParallel = TRUE, ...) {

  len <- length(attributes)
  fun <- match.fun(fun)
  type <- match.arg(type)
  isForward <- type == "forward"
  best <- list(result = -Inf, attrs = rep(as.numeric(!isForward), len))

  if (!isForward) {
    best$result <- fun(attributes[as.logical(best$attrs)], data)
  }

  allResults <- NULL

  repeat {
    children <- get_children(best$attrs, type)
    if (is.null(children)) {
      break
    }

    iterChild <- iter(children, by = "row")

    if (allowParallel && getDoParRegistered()) {
      `%op%` <- `%dopar%`
      if (getDoParName() == "doSNOW") {
        tpb <- txtProgressBar(max = iterChild$length, style = 3)
        progress <- function(n) {
          setTxtProgressBar(pb = tpb, value = n)
        }
        opts <- list(progress = progress)
      } else {
        opts <- NULL
      }
    } else {
      `%op%` <- `%do%`
      opts <- NULL
    }

    childrenResults <- foreach(it = iterChild, .options.snow = opts, ...) %op% {
      fun(attributes[as.logical(as.numeric(it))], data)
    }

    maxIdx <- which.max(childrenResults)

    if (childrenResults[[maxIdx]] > best$result) {
      best$result <- childrenResults[[maxIdx]]
      best$attrs <- children[maxIdx, ]
      allResults <- rbind(allResults, c(best$attrs, best$result))
    } else {
      break
    }
  }

  allResults <- setNames(as.data.frame(allResults),
                         c(attributes, "values"))
  bestResult <- tail(allResults, 1)
  res <- list(bestResult = bestResult,
              allResults = allResults,
              fun = fun)

  return(res)
}

#' General Feature Searching Function
#'
#' Convenience function wrapper for greedy and exhaustive algorithms for
#' searching atrribute subset space.
#'
#' @param attributes Character vector with attributes names.
#' @param fun Function to evaluate. See Details.
#' @param data Data set for fun function.
#' @param mode String to determine which search to perform
#' @param type Argument for \code{mode = "greedy"}. Algorithm used for search.
#' Possible values are \code{forward} and \code{backward}.
#' @param subsetsSizes Argument for \code{mode = "exhaustive"}. Vector of sizes
#' of attributes subsets.
#' @param allowParallel Allow parallelization.
#' @param \dots Other arguments passed to foreach function.
#'
#' @author Zygmunt Zawadzki \email{zygmunt.zawadzki@@gmail.com}
#' @author Krzysztof Slomczynski \email{krzysztofslomczynski@@gmail.com}
#'
#' @importFrom foreach foreach %dopar% %do% getDoParRegistered getDoParName
#' @importFrom iterators iter
#' @importFrom stats setNames
#' @importFrom utils setTxtProgressBar txtProgressBar tail
#'
#' @examples
#'
#' # Enable parallelization in examples
#'  library(doSNOW) # doSNOW has an option for progress bar
#'  cl <- makeCluster(2)
#'  registerDoSNOW(cl)
#'
#' # Close at the end
#' # stopCluster(cl)
#' # registerDoSEQ()
#'
#' # 1) Evaluator from FSelector package.
#' evaluator <- function(subset, data, dependent = names(iris)[5]) {
#'   library(rpart)
#'   k <- 5
#'   splits <- runif(nrow(data))
#'   results <- sapply(1:k, function(i) {
#'     test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#'     train.idx <- !test.idx
#'     test <- data[test.idx, , drop = FALSE]
#'     train <- data[train.idx, , drop = FALSE]
#'     tree <- rpart(to_formula(subset, dependent), train)
#'     error.rate <- sum(test[[dependent]] != predict(tree, test, type = "c")) /
#'     nrow(test)
#'     return(1 - error.rate)
#'   })
#'   return(mean(results))
#' }
#'
#' # Default greedy search.
#' system.time(feature_search(attributes = names(iris)[-5],
#'                      fun = evaluator,
#'                      data = iris))
#' system.time(feature_search(attributes = names(iris)[-5],
#'                      fun = evaluator,
#'                      data = iris,
#'                      allowParallel = FALSE))
#'
#' # Optional exhaustive search.
#' system.time(feature_search(attributes = names(iris)[-5],
#'                      fun = evaluator,
#'                      data = iris,
#'                      mode = "exhaustive"))
#' system.time(feature_search(attributes = names(iris)[-5],
#'                      fun = evaluator,
#'                      data = iris,
#'                      mode = "exhaustive",
#'                      allowParallel = FALSE))
#'
#' # 2) Maximize R^2 statistics in the linear regression model/problem.
#'
#' evaluator_R2_lm <- function(attributes, data, dependent = names(iris)[1]) {
#'   summary(
#'     lm(to_formula(attributes, dependent), data = data)
#'   )$r.squared
#' }
#'
#' feature_search(attributes = names(iris)[-1], fun = evaluator_R2_lm, data = iris,
#'          mode = "exhaustive")
#'
#' # 3) Optimize BIC crietion in generalized linear model.
#' # Aim of Bayesian approach it to identify the model with the highest
#' # probability of being the true model. - Kuha 2004
#'
#' utils::data(anorexia, package = "MASS")
#'
#' evaluator_BIC_glm <- function(attributes, data, dependent = "Postwt") {
#'   extractAIC(
#'     fit = glm(to_formula(attributes, dependent), family = gaussian,
#'               data = data),
#'     k = log(nrow(data))
#'   )[2]
#' }
#'
#' feature_search(attributes = c("Prewt", "Treat", "offset(Prewt)"),
#'          fun = evaluator_BIC_glm,
#'          data = anorexia,
#'          mode = "exhaustive")
#'
#' # Close parallelization
#' stopCluster(cl)
#' registerDoSEQ()
#'
#' @export
feature_search <- function(attributes, fun, data, mode = c("greedy", "exhaustive"),
                     type = c("forward", "backward"),
                     subsetsSizes = 1:length(attributes), allowParallel = TRUE,
                     ...) {

  call <- match.call()
  mode <- match.arg(mode)
  if (mode == "greedy") {
    type <- match.arg(type)
    output <- greedy_search(attributes = attributes, fun = fun, data = data,
                            type = type, allowParallel = allowParallel, ...)
  } else if (mode == "exhaustive") {
    output <- exhaustive_search(attributes = attributes, fun = fun, data = data,
                                subsetsSizes = subsetsSizes,
                                allowParallel = allowParallel, ...)
  }

  output <- c(output, call = call)

  return(output)
}
