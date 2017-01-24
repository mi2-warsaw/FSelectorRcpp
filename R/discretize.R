#' @export
mdlControl <- function() {
  params <- list(method = "MDL")
  attr(params, "class") <- c("mdlControl", "discretizationControl", "list")
  params
}

#' @export
equalsizeControl <- function(k = 10) {
  params <- list(method = "EQUAL_SIZE", k = k)
  attr(params, "class") <- c("equalsizeControl",
                             "discretizationControl",
                             "list")
  params
}

################ Discretization

#' Discretization
#'
#' Discretize a range of numeric attributes in the dataset into nominal attributes. Discretization is by \code{Minimum Description Length} (MD)L method.
#'
#' @param x The explanatory continuous variables to be discretized or formula.
#' @param y The dependent variable for supervised discretization
#' @param control The \code{control} object containing the parameters for discretisation algorithm.
#' @param keepAll tmp
#'
#' @references
#' U. M. Fayyad and K. B. Irani. Multi-Interval Discretization of Continuous-Valued Attributes for Classi-
#' fication Learning. In 13th International Joint Conference on Uncertainly in Artificial Intelligence(IJCAI93),
#' pages 1022–1029, 1993.
#'
#' @examples
#'
#' discretize(iris[[1]], iris[[5]])
#'
#' discretize(Species ~ ., iris)
#'
#' \dontrun{
#' # the same results
#' library(RWeka)
#' RWeka::Discretize(Species~Sepal.Length, data = iris)[, 1] -> Rweka_disc_out
#' FSelectorRcpp::discretize(iris$Sepal.Length, iris$Species) ->FSelectorRcpp_disc_out
#' table(Rweka_disc_out,FSelectorRcpp_disc_out)
#' # but faster method
#' library(microbenchmark)
#' microbenchmark(discretize(iris$Sepal.Length, iris$Species),
#'                Discretize(Species~Sepal.Length, data = iris))
#'
#' }
#'
#' @author Zygmunt Zawadzki , \email{zygmunt.zawadzki@@gmail.com}
#'
#' @export
discretize <- function(x, y, control = mdlControl(), keepAll = FALSE) {
  UseMethod("discretize", x)
}

#' @export
discretize.default <- function(x, y, control = mdlControl(), keepAll = FALSE) {
  stop(sprintf("Object of class %s is not supported!", class(x)[1]))
}

#' @export
discretize.formula <- function(x, y, control = mdlControl(), keepAll = FALSE) {
  formula <- formula2names(x, y)

  data <- y
  yy <- y[[formula$y]]

  colClasses <- sapply(data, is.numeric)
  colClasses <- colClasses[formula$x]

  if (all(!colClasses)) {
    stop("No columns of numeric classes!")
  } else if (!all(colClasses)) {
    warning(paste("Columns with classes other than numeric will be skipped!\n",
                  "\n",
                  " Skipped columns:",
                  paste(names(colClasses)[!colClasses], collapse = ", ")))

    colClasses <- colClasses[colClasses]
  }

  columnsToDiscretize <- names(colClasses)

  for (col in columnsToDiscretize) {
    res <- discretize_cpp(data[[col]], yy, control)

    if (!is.null(attr(res,"SplitValues"))) {
      # ini case of no split points
      splitVals <- attr(res, "SplitValues")
      levels(res) <- levels(cut(splitVals,splitVals))
    }

    data[[col]] <- res
  }

  if (!keepAll) {
    data <- data[, c(columnsToDiscretize, formula$y)]
  }

  return(data)
}

#' @export
discretize.data.frame <- function(x, y, call = match.call(),
                                  control = mdlControl(), keepAll = FALSE) {
  if (!is.data.frame(y)) {
    y <- format_handler(call$y, y)
  }
  y <- cbind(y, x)
  if (any(table(colnames(y)) != 1)) {
    stop("Duplicated columns!")
  }
  x <- formula(paste0(colnames(y)[1], "~."))

  discretize.formula(x = x, y = y, control = control, keepAll = keepAll)
}

#' @export
discretize.numeric <- function(call, x, y, control = mdlControl(), keepAll = FALSE) {
  call <- match.call()
  x <- format_handler(call$x, x)
  discretize.data.frame(call = call, x = x, y = y, control = control,
                        keepAll = keepAll)
}

#' @export
discretize.list <- discretize.numeric
