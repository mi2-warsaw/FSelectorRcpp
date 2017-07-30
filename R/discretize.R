#' Discretization
#'
#' Discretize a range of numeric attributes in the dataset into nominal
#' attributes. \code{Minimum Description Length} (MDL) method is set as the default
#' control. There is also available \code{equalsizeControl} method.
#'
#' @param x Explanatory continuous variables to be discretized or a \link{formula}.
#' @param y Dependent variable for supervised discretization or a \link{data.frame} when \code{x} ia a \link{formula}.
#' @param control \code{discretizationControl} object containing the parameters for
#'   discretization algorithm. Possible inputs are \code{mdlControl} or \code{equalsizeControl}, so far. If passed as a list, the first element is used.
#' @param all Logical indicating if a returned \link{data.frame} should contain factor features that were not discretize.
#' (Example: should \code{Species} be returned, when you pass \code{iris} and discretize all continuous features.)
#' @param call Keep as \code{NULL}. Inner method parameter for consistency.
#'
#' @references U. M. Fayyad and K. B. Irani. Multi-Interval Discretization of
#'   Continuous-Valued Attributes for Classification Learning. In 13th
#'   International Joint Conference on Uncertainly in Artificial
#'   Intelligence(IJCAI93), pages 1022-1029, 1993.
#'
#' @examples
#'
#' # vectors
#' discretize(x = iris[[1]], y = iris[[5]])
#'
#' # list and vector
#' discretize(x = list(iris[[1]], iris$Sepal.Width), y = iris$Species)
#'
#' # formula input
#' discretize(x = Species ~ ., y = iris)
#' discretize(Species ~ ., iris)
#'
#' \dontrun{
#' # Same results
#' library(RWeka)
#' Rweka_disc_out <- RWeka::Discretize(Species ~ Sepal.Length, iris)[, 1]
#' FSelectorRcpp_disc_out <- FSelectorRcpp::discretize(Species ~ Sepal.Length,
#'                                                     iris)[, 1]
#' table(Rweka_disc_out, FSelectorRcpp_disc_out)
#' # But faster method
#' library(microbenchmark)
#' microbenchmark(FSelectorRcpp::discretize(Species ~ Sepal.Length, iris),
#'                RWeka::Discretize(Species ~ Sepal.Length, iris))
#'
#' }
#'
#' @author Zygmunt Zawadzki \email{zygmunt@zstat.pl}
#' @importFrom stats formula
#' @export
discretize <- function(x, y, control = list(mdlControl(), equalsizeControl()),
                       all = FALSE, call = NULL) {
  UseMethod("discretize", x)
}

#' @export
discretize.default <- function(x, y,
                               control = list(mdlControl(), equalsizeControl()),
                               all = FALSE, call = NULL) {
  stop(sprintf("Object of class %s is not supported!", class(x)[1]))
}

#' @export
discretize.formula <- function(x, y,
                               control = list(mdlControl(), equalsizeControl()),
                               all = FALSE, call = NULL) {
  formula <- formula2names(x, y)
  data <- y
  yy <- y[[formula$y]]

  if (!("discretizationControl" %in% class(control))) {
    control <- control[[1]]
  }

  colClasses <- sapply(data, is.numeric)
  colClasses <- colClasses[formula$x]

  if (all(!colClasses)) {
    stop("No columns of numeric classes!")
  } else if (any(!colClasses)) {

    if (!all) {
      warning(
        paste("Columns with classes other than numeric will be skipped!\n",
              "\n",
              " Skipped columns:",
              paste(names(colClasses)[!colClasses], collapse = ", "))
      )
    }

    colClasses <- colClasses[colClasses]
  }

  columnsToDiscretize <- names(colClasses)

  for (col in columnsToDiscretize) {
    res <- discretize_cpp(data[[col]], yy, control)

    if (!is.null(attr(res, "SplitValues"))) {
      # ini case of no split points
      splitVals <- attr(res, "SplitValues")
      levels(res) <- levels(cut(splitVals, splitVals))
    }

    data[[col]] <- res
  }

  if (!all) {
    data <- data[, c(formula$y, columnsToDiscretize)]
  }

  return(data)
}

#' @export
discretize.data.frame <- function(x, y,
                                  control = list(mdlControl(),
                                                 equalsizeControl()),
                                  all = FALSE, call = match.call()) {
  if (!is.data.frame(y)) {
    y <- format_handler(call$y, y)
  }
  y <- cbind(y, x)
  if (any(table(colnames(y)) != 1)) {
    stop("Duplicated columns!")
  }
  x <- formula(paste0(colnames(y)[1], "~.")) #nolint

  if (!("discretizationControl" %in% class(control))) {
    control <- control[[1]]
  }

  discretize.formula(x = x, y = y, control = control, all = all)
}
#' @export
discretize.numeric <- function(x, y,
                               control = list(mdlControl(), equalsizeControl()),
                               all = FALSE, call = NULL) {
  call <- match.call()
  x <- format_handler(call$x, x)

  if (!("discretizationControl" %in% class(control))) {
    control <- control[[1]]
  }

  discretize.data.frame(x = x, y = y, control = control,
                        all = all, call = call)
}

#' @export
discretize.list <- discretize.numeric

#' @export
discretize.matrix <- discretize.numeric

#' @rdname discretize
#' @export
mdlControl <- function() {
  params <- list(method = "MDL")
  attr(params, "class") <- c("mdlControl", "discretizationControl", "list")
  params
}

#' @rdname discretize
#' @param k Number of partitions.
#' @export
equalsizeControl <- function(k = 10) {
  stopifnot(k > 0)
  k <- floor(k)
  params <- list(method = "EQUAL_SIZE", k = k)
  attr(params, "class") <- c("equalsizeControl",
                             "discretizationControl",
                             "list")
  params
}
