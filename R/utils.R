#' Formula to variables names
#'
#' Converts formula to character vector
#'
#'
#' @noRd
#'
formula2names = function(formula, data)
{
  # lFormula = as.list(formula)
  #
  # y = as.character(lFormula[[2]])
  #
  # if(as.list(lFormula[[3]])[[1]] == ".")
  # {
  #   cnames = colnames(data)
  #   x = cnames[!cnames == y]
  # } else
  # {
  #   x = as.list(lFormula[[3]])[-1]
  #   x = sapply(x, as.character)
  # }
  y = formula[[2]]
  x = attr(stats::terms(formula, data=data), "term.labels")

  list(y = as.character(y), x = x)
}



