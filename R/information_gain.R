
#' Entropy-based filters
#'
#' Entropy-based filters
#'
#' @param formula description of a model
#' @param data data to process
#' @param type method name.
#'
#' @return data.frame
#' @examples
#'
#' information_gain(Species ~ ., data = iris)
#' information_gain(Species ~ ., data = iris, type = "gainratio")
#' information_gain(Species ~ ., data = iris, type = "symuncert")
#'
information_gain = function(formula, data, type = c("infogain", "gainratio", "symuncert"))
{
  type = match.arg(type)

  formula = formula2names(formula,data)

  y = data[[formula$y]]

  if(anyNA(y))
  {
    stop("FSelector does not support NA in dependent variable")
  }

  if(!is.factor(y))
  {
    y = factor(y)
  }

  values = information_gain_cpp(data[formula$x], y)

  classEntropy = fs_entropy1d(y)

  attrEntropy  = values$entropy
  jointEntropy = values$joint

  results = classEntropy + attrEntropy - jointEntropy

  if(type == "gainration")
  {
    results = results / attrEntropy
  } else if(type == "symuncert")
  {
    results = 2 * results / (attrEntropy	+ classEntropy)
  }


  data.frame(importance = results, row.names = formula$x)
}
