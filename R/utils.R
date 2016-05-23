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
  cat("\t",paste(get_best_attributes(x), collapse = " + "))
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

#### create children ----
get_children = function (parent, direction = c("forward", "backward", "both"),
          omit.func = NULL)
{
  # adopted from FSelector package

  direction = match.arg(direction)
  if (!is.null(omit.func))
  {
    omit.func = match.fun(omit.func)
  }

  cols = length(parent)
  if (cols <= 0)
    stop("Parent attribute set cannot be empty.")

  m1 = NULL
  m2 = NULL

  if (direction == "forward" || direction == "both")
  {
    rows = cols - sum(parent)
    if (rows > 0)
    {
      m1 = matrix(parent, ncol = cols, nrow = rows, byrow = TRUE)
      CurrRow = 1
      CurrCol = 1
      while(CurrCol <= cols && CurrRow <= rows)
      {
        if (m1[CurrRow, CurrCol] == 0) {
          m1[CurrRow, CurrCol] = 1
          CurrRow = CurrRow + 1
        }
        CurrCol = CurrCol + 1
      }
    }
  }

  if (direction == "backward" || direction == "both")
  {
    rows = sum(parent)
    if (rows > 1) {
      m2 = matrix(parent, ncol = cols, nrow = rows, byrow = TRUE)
      CurrRow = 1
      CurrCol = 1
      while(CurrCol <= cols && CurrRow <= rows)
      {
        if (m2[CurrRow, CurrCol] == 1)
        {
          m2[CurrRow, CurrCol] = 0
          CurrRow = CurrRow + 1
        }
        CurrCol = CurrCol + 1
      }
    }
  }

  m = rbind(m1, m2)
  if (is.null(m))
    return(m)
  if (!is.null(omit.func))
  {
    RowToOmit = apply(m, 1, omit.func)
    return(m[!RowToOmit, , drop = FALSE])
  } else
  {
    return(m)
  }
}

