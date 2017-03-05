#' Select Attributes by Score Depending on the Cutoff
#'
#' Select attributes by their score/rank/weights, depending on the cutoff that may be specified
#' by the percentage of the highest ranked attributes or by the number of the highest ranked attributes.
#'
#' @param attrs A \link{data.frame} with attributes' importance.
#' @param k A numeric. For \code{k >= 1} it takes \code{floor(k)} and then it indicates how many attributes to
#' take with the highest attribute rank (chooses k best attributes).
#' For \code{0 < k < 1} it stands for the percent of top attributes to take
#' (chooses best k * 100\% of attributes).
#'
#' @author Damian Skrzypiec \email{damian.j.skrzypiec@@gmail.com} and
#' Zygmunt Zawadzki \email{zygmunt@zstat.pl}
#'
#' @examples
#'
#' x <- information_gain(Species ~ ., iris)
#' cut_attrs(attrs = x)
#' cut_attrs(attrs = x, k = 1)

#' @export
cut_attrs <- function(attrs, k = 0.5) {
  if (!is.data.frame(attrs)) {
    stop("attrs must be a data.frame!")
  }
  if (length(attrs) != 2) {
    stop("attrs must have 2 columns!")
  }

  classes <- unname(sapply(attrs, class))

  if (!("numeric" %in% classes)) {
    stop("The class of importance column must be numeric!")
  }
  if ("factor" %in% classes) {
    factorAttr <- which(classes == "factor")
    attrs[factorAttr] <- as.character(attrs[factorAttr])
  }

  attributes <- which(classes == "character")
  importance <- which(classes == "numeric")
  nAttrs <- nrow(attrs)

  if (k < 1/nAttrs) {
    warning("k is too small. Selecting one of the attributes.")
    k <- 1
  } else if (k > nAttrs) {
    warning("k is bigger than number of attributes. Selecting all of them.")
    k <- nAttrs
  } else if (1 < k) {
    k <- floor(k)
  }

  cutOff_k(attrs[[attributes]], attrs[[importance]], k)
}
