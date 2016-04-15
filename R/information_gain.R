
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

  results = information_type(classEntropy, attrEntropy, jointEntropy, type)

  data.frame(importance = results, row.names = formula$x)
}


#' Entropy-based filters
#'
#' Entropy-based filters
#'
#' @param x sparse matrix
#' @param y dependent variable
#' @param type method name.
#'
#' @return data.frame
#' @examples
#'
#' library(Matrix)
#' i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
#' x <- sparseMatrix(i, j, x = x)
#' y = c(1,1,1,1,2,2,2,2)
#'
sp_information_gain = function(x, y, type = c("infogain", "gainratio", "symuncert"))
{
  type = match.arg(type)
  values = sparse_information_gain_cpp(x,y)
}


information_type = function(classEntropy,
                            attrEntropy,
                            jointEntropy,
                            type = c("infogain", "gainratio", "symuncert"))
{
  results = classEntropy + attrEntropy - jointEntropy

  if(type == "gainratio")
  {
    results = results / attrEntropy
  } else if(type == "symuncert")
  {
    results = 2 * results / (attrEntropy	+ classEntropy)
  }

  results
}
