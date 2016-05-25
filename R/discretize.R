#' Discretization
#'
#' Discretize a range of numeric attributes in the dataset into nominal attributes. Discretization is by MDL method.
#'
#' @param x x
#' @param y y
#'
#' @export
#'
#' @examples
#'
#' discretize(iris[[1]], iris[[5]])
#'
discretize = function(x, y)
{
  UseMethod("discretize", x)
}

#' @export
discretize.default = function(x, y)
{
  stop(sprintf("Object of class %s is not supported!", class(x)[1]))
}

#' @export
discretize.numeric = function(x, y)
{
  discretize_cpp(x, y)
}

discretize.formula = function(x, y)
{
  stop("Not implemented yet!")
}

discretize.data.frame = function(x, y)
{
  stop("Not implemented yet!")
}

