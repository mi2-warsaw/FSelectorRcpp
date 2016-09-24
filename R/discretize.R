#' @export
mdlControl = function()
{
  params = list(method = "MDL")
  attr(params, "class") = c("mdlControl", "discretizationControl", "list")
  params
}

#' @export
equalsizeControl = function(k = 10)
{
  params = list(method = "EQUAL_SIZE", k = k)
  attr(params, "class") = c("equalsizeControl", "discretizationControl", "list")
  params
}

################ Discretisation

#' Discretization
#'
#' Discretize a range of numeric attributes in the dataset into nominal attributes. Discretization is by MDL method.
#'
#' @param x x
#' @param y y
#' @param control control object containing the parameters for discretisation algorithm.
#'
#' @export
#'
#' @examples
#'
#' discretize(iris[[1]], iris[[5]])
#'
#' discretize(Species ~ ., iris)
#'
discretize = function(x, y, control = mdlControl())
{
  UseMethod("discretize", x)
}

#' @export
discretize.default = function(x, y, control = mdlControl())
{
  stop(sprintf("Object of class %s is not supported!", class(x)[1]))
}

#' @export
discretize.numeric = function(x, y, control = mdlControl())
{
  discretize_cpp(x, y, control)
}

#' @export
discretize.formula = function(x, y, control = mdlControl())
{
  formula = formula2names(x, y)

  data = y
  yy   = y[[formula$y]]

  colClasses = sapply(data,  is.numeric)
  colClasses = colClasses[formula$x]

  if(!all(colClasses))
  {
    warning(sprintf("Columns with classes other than numeric will be skipped! \n
 Skipped columns:
  %s",paste(names(colClasses)[!colClasses], collapse = ", ")))

    colClasses = colClasses[colClasses]
  }

  columnsToDiscretize = names(colClasses)

  for(col in columnsToDiscretize)
  {
    data[[col]] = discretize(data[[col]], yy, control)
  }

  return(data)
}

#' @export
discretize.data.frame = function(x, y)
{
  stop("Not implemented yet!")
}

