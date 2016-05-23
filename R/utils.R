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

#' @export
to_formula = function(attr, class)
{
  as.formula(paste(class, paste(attr, collapse = " + "), sep = " ~ "))
}

#### fs_get_best_attributes ----
#' @export
get_best_attributes = function(x) UseMethod("get_best_attributes")

#' @export
get_best_attributes.ExhaustiveSearchResult = function(x)
{
  x$attributes[as.logical(x$bestAttr)]
}

#### print functions ----

#' @export
print.ExhaustiveSearchResult = function(x, ...)
{
  cat("Exhaustive Search Result:\n\n")
  cat("\t",paste(fs_get_best_attributes(x), collapse = " + "))
  cat("\n\n\n")
  if(!is.null(x$allResult))
  {
    cat('  Results for other attributes subsamples are avaiable.
  You can extract them with x[["allResult"]]\n')
  } else
  {
    cat("  Results for other attributes subsamples are not avaiable.
  You can get them by rerunning fs_exhaustive_search with keepAll = TRUE\n")
  }
}
