#' Selection Cutoffs for Ranked Attributes
#'
#' @param attrs data.frame with attributes' importance.
#' @param k A numeric. For \code{k > 1} it indicates how many attributes to take with the highest attribute rank (chooses k best attributes). For \code{0 < k < 1} it stands for the percent
#' of top attributes to take (chooses best k * 100\% of attributes).
#'
#' @author Damian Skrzypiec \email{damian.j.skrzypiec@@gmail.com} and Zygmunt Zawadzki \email{zygmunt@zstat.pl}
#'
#' @export
#' @examples
#'
#' x = information_gain(Species ~ ., iris)
#' cutOff_attrs(x)
#' cutOff_attrs(x, k = 1)
#'
cutOff_attrs <- function(attrs, k = 0.5) {
  if (!is.data.frame(attrs)) {
    stop("attrs must be a data.frame!")
  }
  cutOff_k(rownames(attrs), attrs[[1]], k)
}
