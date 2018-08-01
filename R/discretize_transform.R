#' Transform a data.frame using split points returned by discretize function.
#'
#' @param disc a result of the \code{\link{discretize}} function.
#' @param data a data.frame to transform using cutpoints from disc.
#' @param dropColumns determine
#'
#' @return
#'
#' A new data.frame with discretized columns using cutpoints
#' from the result of discretize function.
#'
#' @export
#' @rdname discretize_transform
#'
#' @examples
#'
#' set.seed(123)
#' idx <- sort(sample.int(150, 100))
#' iris1 <- iris[idx, ]
#' iris2 <- iris[-idx, ]
#' disc <- discretize(Species ~ ., iris)
#' head(discretize_transform(disc, iris2))
#'
#' # Chain discretization:
#' ir1 <- discretize(Species ~ Sepal.Length, iris1)
#' ir2 <- discretize(Species ~ Sepal.Width, ir1, control = equalsizeControl(3))
#' ir3 <- discretize(Species ~ Petal.Length, ir2, control = equalsizeControl(5))
#'
#' ## note that Petal.Width is untouched:
#' head(discretize_transform(ir3, iris2))
#'
#' ## extract_discretize_transformer
#' discObj <- extract_discretize_transformer(ir3)
#' head(discretize_transform(discObj, iris2))
#'
discretize_transform <- function(disc, data, dropColumns = NA) {
  UseMethod("discretize_transform", disc)
}

#' @export
discretize_transform.data.frame <- function(disc, data, dropColumns = NA) {
  x <- extract_discretize_transformer(disc)
  discretize_transform(x, data, dropColumns = dropColumns)
}

#' @export
discretize_transform.FsDiscretizeTransformer <- function(disc, data, dropColumns = NA) {

  splitPoints <- disc$fsSplitPointsList
  cols <- names(splitPoints)[names(splitPoints) %in% names(data)]

  for (nm in cols) {

    sp <- splitPoints[[nm]]
    if (!anyNA(sp)) {
      data[[nm]] <- cut(data[[nm]], sp, ordered_result = TRUE)
    } else {
      if (is.na(dropColumns) || dropColumns) {
        data[[nm]] <- NULL
      } else {
        data[[nm]] <- NA
      }
    }
  }

  attr(data, "fsSplitPointsList") <- splitPoints

  if (isTRUE(dropColumns)) {
    data <- data[intersect(disc$colNames, colnames(data))]
  }

  data
}

#' @export
#' @rdname discretize_transform
extract_discretize_transformer <- function(disc) {
  x <- list(
    colNames = colnames(disc),
    fsSplitPointsList = attr(disc, "fsSplitPointsList")
  )

  class(x) <- c("FsDiscretizeTransformer", "list")
  x
}

#' @export
print.FsDiscretizeTransformer <- function(x, ...) {

  maxWidth <- round(pmax(getOption("width") * 0.7, 30))

  cutpoints <-
    mapply(
      function(x, y)
        paste(x, paste(y, collapse = ", "), sep = ": "),
      names(x$fsSplitPointsList),
      x$fsSplitPointsList
    )

  cutpoints <- ifelse(nchar(cutpoints) < maxWidth, cutpoints, paste(substr(cutpoints, 1, maxWidth - 3), "...", sep = ""))
  cutpoints <- paste("  ", cutpoints, "\n", sep = "")
  cat("FsDiscretizeTransformer\n\nCutpoints:\n", sep = "")
  cat(cutpoints, sep = "")

  info <- paste("FsDiscretizeTransformer allows to",
                "discretize data using",
                "discretize_transform(disc, newData) function.",
                sep = " ")

  info <- paste(strwrap(info, maxWidth), collapse = "\n")
  cat("\n", info, sep = "")
}
