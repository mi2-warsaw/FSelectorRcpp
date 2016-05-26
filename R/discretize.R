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
#' discretize(Species ~ ., iris)
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

#' @export
discretize.formula = function(x, y)
{
  formula = FSelectorRcpp:::formula2names(x, y)

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
    data[[col]] = discretize(data[[col]], yy)
  }

  return(data)
}

#' @export
discretize.data.frame = function(x, y)
{
  stop("Not implemented yet!")
}

