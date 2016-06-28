#' Selection Cutoffs for Ranked Attributes
#'
#' @param attrs data.frame with attributes' importance or vector with attributes' names.
#' @param k A numeric. For \code{k > 1} it indicates how many attributes to take with the highest attribute rank (chooses k best attributes). For \code{0 < k =< 1} it stands for the percent
#' @param rank vector with attributes' ranks to cut off (used when attrs is a vector).
#'
#' of top attributes to take (chooses best k * 100% of attributes). For \code{k = 0} it selects top attributes
#' before the biggest gap in differences of scores for neighbouring attributes
#' (chooses a subset of attributes which are significantly better than other.)
#'
#' @author Damian Skrzypiec \email{damian.j.skrzypiec@@gmail.com} and Zygmunt Zawadzki \email{zygmunt@zstat.pl}
#'
#' @export
#' @examples
#'
#' x = information_gain(Species ~ ., iris)
#' cutOff_attrs(rownames(x), 2, x[[1]])
#' cutOff_attrs(x)
#'
cutOff_attrs = function(attrs, k = 0.5, rank = NULL)
{
  UseMethod("cutOff_attrs")
}

#' @export
cutOff_attrs.character = function(attrs, k = 0.5, rank = NULL)
{
  if(is.null(rank))
  {
    stop("rank is not provided!")
  }

  cutOff_k(attrs, rank, k)
}

#' @export
cutOff_attrs.data.frame = function(attrs, k = 0.5, rank = NULL)
{
  if(!is.null(rank))
  {
    warning("rank is provided but will be not used!")
  }

  cutOff_k(rownames(attrs), attrs[[1]], k)
}


