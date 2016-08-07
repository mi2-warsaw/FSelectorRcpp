utils::globalVariables("it")

#' Exhaustive search
#'
#' The algorithm for searching atrribute subset space.
#'
#' @param attributes character vector with attributes names
#' @param fun function to evaluate. See Details.
#' @param data data set for fun function.
#' @param subsetsSizes sizes of attributes subsets.
#' @param singleAttr singleAttr
#' @param keepAll keepAll
#' @param allowParallel allowParallel,
#' @param randomSubsetsNumber number of random subsets. Used only if greater than zero.
#' @param \dots other arguments passed to foreach
#'
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom iterators iter
#' @examples
#'
#'
#' ### enable parallelization in examples
#'
#'  library(doParallel)
#'  cl = makeCluster(2)
#'  registerDoParallel(cl)
#' # close at the end
#' # stopCluster(cl)
#' # registerDoSEQ()
#'
#' ####################################
#' ## 1) evaluator from FSelector package
#' ####################################
#' evaluator = function(subset, data)
#' {
#'   library(rpart)
#'   k <- 5
#'   splits <- runif(nrow(data))
#'   results = sapply(1:k, function(i) {
#'   test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#'   train.idx <- !test.idx
#'   test <- data[test.idx, , drop=FALSE]
#'   train <- data[train.idx, , drop=FALSE]
#'   tree <- rpart(to_formula(subset, "Species"), train)
#'   error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
#'   return(1 - error.rate)
#'    })
#'   return(mean(results))
#' }
#'
#'
#'  system.time(exhaustive_search(names(iris)[-5], evaluator, iris))
#'  system.time(exhaustive_search(names(iris)[-5],
#'                                evaluator,
#'                                iris,
#'                                allowParallel = FALSE))
#'  system.time(exhaustive_search(names(iris)[-5],
#'                                evaluator,
#'                                iris,
#'                                allowParallel = FALSE,
#'                                randomSubsetsNumber = 5))
#'
#'
#'
#' ################################
#' ## 2) maximize R^2 statistics in
#' ## the linear regression model/problem
#' ################################
#'
#' evaluator_R2_lm = function(attributes, data, dependent = names(iris)[1])
#' {
#'   summary(
#'     lm(to_formula(attributes, dependent), data = data)
#'   )$r.squared
#' }
#'
#'
#' system.time(exhaustive_search(names(iris)[-1], fun = evaluator_R2_lm, data =iris))
#'
#'
#'
#' ##############################
#' ## 3) optimize BIC crietion in
#' ## generalized linear model
#' ##
#' ## Aim of Bayesian approach it to identify the
#' ## model with the highest probability of being
#' ## the true model. - Kuha 2004
#' ##############################
#' utils::data(anorexia, package = "MASS")
#'
#' evaluator_BIC_glm <- function(attributes, data, dependent = "Postwt")
#' {
#'   extractAIC(
#'     glm(to_formula(attributes, dependent),
#'         family = gaussian, data = data),
#'     k = log(nrow(data))
#'   )[2]
#' }
#'
#' exhaustive_search(c("Prewt", "Treat", "offset(Prewt)"),
#'                   fun = evaluator_BIC_glm,
#'                   data = anorexia)
#'
#' ### close parallelization
#' stopCluster(cl)
#' registerDoSEQ()
#'
#'
#' @export
#'
exhaustive_search = function(attributes,
                             fun,
                             data,
                             subsetsSizes = length(attributes),
                             singleAttr = FALSE,
                             keepAll = TRUE,
                             allowParallel = TRUE,
                             randomSubsetsNumber = 0, ...)
{
  len = length(attributes)
  if (len == 0)
    stop("attributes length must be > 0")
  fun = match.fun(fun)


  bestVal = -Inf
  bestAttr = NULL

  allCombn = NULL
  allResult = NULL

  if(length(subsetsSizes) == 1 & !singleAttr)
  {
    subsetsSizes = 1:subsetsSizes
  }

  `%op%` = ifelse(allowParallel, `%dopar%`, `%do%`)

  for(size in subsetsSizes)
  {
    if(randomSubsetsNumber > len)
    {
      if(choose(len, size) <= randomSubsetsNumber)
      {
        childComb = combn(1:len, size) # use all subsets
      } else
      {
        childComb = replicate(randomSubsetsNumber, {sample.int(len, size, replace = FALSE)})
      }
    } else
    {
      childComb = combn(1:len, size)
    }

    zeroes = rep(0, len)
    childComb = apply(childComb, 2, function(i) { x = zeroes; x[i] = 1; x}  )
    childComb = t(childComb)

    matIter = iter(childComb, by = "row")

    result = foreach(it = matIter, ...) %op%
    {
      fun(attributes[as.logical(as.numeric(it))], data)
    }

    result = unlist(result)

    if(keepAll)
    {
      allResult = c(allResult, result)
      allCombn  = rbind(allCombn, childComb)
    }

    maxIdx = which.max(result)

    if(result[maxIdx] > bestVal)
    {
      bestVal  = result[maxIdx]
      bestAttr = childComb[maxIdx,]
    }

  }

  if(keepAll)
  {
    res = list(result = bestVal, bestAttr = bestAttr, allResult = cbind(allCombn, values = allResult), attributes = attributes)
  } else
  {
    res = list(result = bestVal, bestAttr = bestAttr, attributes = attributes)
  }

  attr(res, "class") = c("ExhaustiveSearchResult","list")

  return(res)
}

#### greedy search ----

#' Greedy Search
#'
#' The algorithm for searching atrribute subset space.
#'
#' @param attributes character vector with attributes names
#' @param fun function to evaluate. See Details.
#' @param data data set for fun function.
#' @param type algorithm used for search. Possible values \code{forward} and \code{backward}.
#' @param allowParallel allowParallel
#' @param \dots other arguments passed to foreach
#'
#' @examples
#'
#' evaluator = function(subset, data)
#' {
#'   library(rpart)
#'   k <- 5
#'   splits <- runif(nrow(data))
#'   results = sapply(1:k, function(i) {
#'   test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#'   train.idx <- !test.idx
#'   test <- data[test.idx, , drop=FALSE]
#'   train <- data[train.idx, , drop=FALSE]
#'   tree <- rpart(to_formula(subset, "Species"), train)
#'   error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
#'   return(1 - error.rate)
#'    })
#'   return(mean(results))
#'  }
#'
#' library(doParallel)
#' cl = makeCluster(2)
#' registerDoParallel(cl)
#'
#' system.time(greedy_search(names(iris)[-5], evaluator, iris))
#' system.time(greedy_search(names(iris)[-5], evaluator, iris, allowParallel = FALSE))
#'
#' stopCluster(cl)
#' registerDoSEQ()
#'
#' @export
greedy_search = function (attributes, fun, data, type = c("forward", "backward"), allowParallel = TRUE, ...)
{
  if (length(attributes) == 0)
    stop("Attributes not specified")

  fun = match.fun(fun)
  type = match.arg(type)

  isForward = type == "forward"

  best = list(result = -Inf, attrs = rep(as.numeric(!isForward),
                                         length(attributes)))
  if(!isForward)
  {
    best$result = fun(attributes[as.logical(best$attrs)], data)
  }

  `%op%` = ifelse(allowParallel, `%dopar%`, `%do%`)

  repeat
  {
    children = get_children(best$attrs, type)
    if (is.null(children)) break;

    iterChild = iter(children, by = "row")


    childrenResults = foreach(it = iterChild, ...) %op%
    {
      fun(attributes[as.logical(it)],data)
    }

    childrenResults = unlist(childrenResults)

    maxIdx = which.max(childrenResults)


    if(childrenResults[maxIdx] > best$result)
    {
      best$result = childrenResults[maxIdx]
      best$attrs  = children[maxIdx, ]
    } else
    {
      break;
    }
  }

  best
}
