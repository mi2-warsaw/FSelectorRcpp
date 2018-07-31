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
discretize_transform <- function(disc, data, dropColumns = NA) {

  splitPoints <- attr(disc, 'fsSplitPointsList')
  cols <- names(splitPoints)[names(splitPoints) %in% names(data)]

  for(nm in cols) {

    sp <- splitPoints[[nm]]
    if(!anyNA(sp)) {
      data[[nm]] <- cut(data[[nm]], sp, ordered_result = TRUE)
    } else {
      if(is.na(dropColumns) || dropColumns) {
        data[[nm]] <- NULL
      } else {
        data[[nm]] <- NA
      }
    }
  }

  attr(data, 'fsSplitPointsList') <- splitPoints

  if(isTRUE(dropColumns)) {
    data <- data[intersect(colnames(disc), colnames(data))]
  }

  data
}
