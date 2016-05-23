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
#' @param allowParallel allowParallel
#' @param \dots other arguments passed to foreach
#'
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom iterators iter
#' @examples
#'
#' # evaluator from FSelector package
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
#'  library(doParallel)
#'  registerDoParallel(cores = 2)
#'  system.time(exhaustive_search(names(iris)[-5], evaluator, iris))
#'  system.time(exhaustive_search(names(iris)[-5], evaluator, iris, allowParallel = FALSE))
#'
#' @export
#'
exhaustive_search = function(attributes, fun, data, subsetsSizes = length(attributes), singleAttr = FALSE, keepAll = TRUE, allowParallel = TRUE, ...)
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
    childComb = combn(1:len, size)
    childComb = apply(childComb, 2, function(i) { x = rep(0, len); x[i] = 1; x}  )
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
#' @examples
#'
#' library(doParallel)
#' registerDoParallel(cores = 2)
#' system.time(greedy_search(names(iris)[-5], evaluator, iris))
#' system.time(greedy_search(names(iris)[-5], evaluator, iris, allowParallel = FALSE))
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
