#' Formula to variables names
#'
#' Converts formula to character vector
#'
#' @param formula
#' @param data
#'
#' @noRd
#'
formula2names = function(formula, data)
{
  lFormula = as.list(formula)

  y = lFormula[[2]]

  if(as.list(lFormula[[3]])[[1]] == ".")
  {
    cnames = colnames(data)
    x = cnames[!cnames == y]
  } else
  {
    x = as.list(lFormula[[3]])[-1]
    x = sapply(x, as.character)
  }

  list(y = y, x = x)
}



