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
#' @param all Logical indicating if a returned \link{data.frame} should contain other features that were not discretized.
#' (Example: should \code{Sepal.Width} be returned, when you pass \code{iris} and discretize \code{Sepal.Length, Petal.Length, Petal.Width}.)
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
#' # use different methods for specific columns
#' ir1 <- discretize(Species ~ Sepal.Length, iris)
#' ir2 <- discretize(Species ~ Sepal.Width, ir1, control = equalsizeControl(3))
#' ir3 <- discretize(Species ~ Petal.Length, ir2, control = equalsizeControl(5))
#' head(ir3)
#'
#' # custom breaks
#' ir <- discretize(Species ~ Sepal.Length, iris,
#'   control = customBreaksControl(breaks = c(0, 2, 5, 7.5, 10)))
#' head(ir)
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
                       all = TRUE, call = NULL) {
  UseMethod("discretize", x)
}

#' @export
discretize.default <- function(x, y,
                               control = list(mdlControl(), equalsizeControl()),
                               all = TRUE, call = NULL) {
  stop(sprintf("Object of class %s is not supported!", class(x)[1]))
}

#' @export
discretize.formula <- function(x, y,
                               control = list(mdlControl(), equalsizeControl()),
                               all = TRUE, call = NULL) {
  formula <- formula2names(x, y)
  data <- y
  yy <- y[[formula$y]]

  if (!("discretizationControl" %in% class(control))) {
    control <- control[[1]]

    if (!"discretizationControl" %in% class(control)) {
      stop(paste("control is not a subclass of discretizationControl.",
        paste("  Please use mdlControl(), customBreaksControl()",
        "or equalsizeControl() functions."),
                 sep = "\n"))
    }
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

  splitPointsList <- list()

  for (col in columnsToDiscretize) {

    if (class(control)[1] == "customBreaksControl") {
      res <- cut(data[[col]], control$breaks, ordered_result = TRUE)
      attr(res, "SplitValues") <- control$breaks

    } else {
      res <- discretize_cpp(data[[col]], yy, control)
    }

    if (anyNA(data[[col]])) {
      res[res == 0] <- NA
    }
    class(res) <- c("ordered", "factor")

    if (!is.null(attr(res, "SplitValues"))) {
      # in case of no split points

      splitVals <- attr(res, "SplitValues")
      levels(res) <- levels(cut(splitVals, splitVals))

      splitPointsList[[col]] <- splitVals
    } else {
      warning(paste(
        sprintf("Cannot find any split points for `%s`.", col),
        "Drops this column.",
        sep = " "
      ))
      res <- NULL
      splitPointsList[[col]] <- NA
    }

    data[[col]] <- res
  }

  if (!all) {
    data <- data[, c(formula$y, columnsToDiscretize)]
  }

  attr(data, "fsSplitPointsList") <- c(
    attr(data, "fsSplitPointsList"),
    splitPointsList
  )

  return(data)
}

#' @export
discretize.data.frame <- function(x, y,
                                  control = list(mdlControl(),
                                                 equalsizeControl()),
                                  all = TRUE, call = match.call()) {

  if (class(y)[[1]] == "formula") {
    discretize.formula(x = y, y = x, control = control, all = all)
  } else {
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
}
#' @export
discretize.numeric <- function(x, y,
                               control = list(mdlControl(), equalsizeControl()),
                               all = TRUE, call = NULL) {
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

#' @rdname discretize
#' @param breaks custom breaks used for partitioning.
#' @export
customBreaksControl <- function(breaks) {
  stopifnot(is.numeric(breaks))
  params <- list(method = "CUSTOM_BREAKS", breaks = breaks)
  attr(params, "class") <- c("customBreaksControl",
                             "discretizationControl",
                             "list")
  params
}

#' Discretize numeric dependent variable
#'
#' Returns discretized dependent variable
#'
#' The observations are segregated into bins in a way that every bin has the
#' same number of them. This function is based on the original one from
#' FSelector package.
#'
#' @noRd
equal_freq_bin <- function(data, bins) {
  bins <- as.integer(bins)

  if (!is.numeric(data)) {
    stop("Data must be numeric!")
  }

  if (bins < 1) {
    stop("Number of bins must be greater than 1!")
  }

  complete <- complete.cases(data)
  ord <- order(data)
  len <- length(data[complete])
  blen <- len / bins
  new_data <- data
  p1 <- 0
  p2 <- 0

  for (i in 1:bins) {
    p1 <- p2 + 1
    p2 <- round(i * blen)
    new_data[ord[p1:min(p2, len)]] <- i
  }

  new_data
}
