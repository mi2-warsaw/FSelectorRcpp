#' Cutoff attributes
#'
#' Select Cutoffs for Ranked Attributes
#'
#' @param attrs data.frame with attributes' importance.
#' @param k A numeric. For \code{k >= 1} it indicates how many attributes to
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
  cutOff_k(rownames(attrs), attrs[[1]], k)
}
