#' Entropy-based filters
#'
#' Entropy-based filters
#'
#' The algorithms find weights of discrete attributes basing on their correlation with continous class attribute.
#'
#' @details
#'
#'  \code{type = "infogain"} is \deqn{H(Class) + H(Attribute) - H(Class, Attribute)}{H(Class) + H(Attribute) - H(Class, Attribute)}.
#'
#'  \code{type = "gainratio"} is \deqn{\frac{H(Class) + H(Attribute) - H(Class, Attribute)}{H(Attribute)}}{(H(Class) + H(Attribute) - H(Class, Attribute)) / H(Attribute)}
#'
#'  \code{type = "symuncert"} is \deqn{2\frac{H(Class) + H(Attribute) - H(Class, Attribute)}{H(Attribute) + H(Class)}}{2 * (H(Class) + H(Attribute) - H(Class, Attribute)) / (H(Attribute) + H(Class))}
#'
#' @param formula description of a model
#' @param data data to process
#' @param type method name.
#' @param x sparse matrix
#' @param y dependent variable
#' @param threads no idea what is it for Zygmunt?
#'
#' @return a data.frame containing the worth of attributes in the first column and their names as row names
#'
#' @author Zygmunt Zawadzki , \email{zygmunt.zawadzki@@gmail.com}
#'
#' @examples
#'
#' information_gain(Species ~ ., data = iris)
#' information_gain(Species ~ ., data = iris, type = "gainratio")
#' information_gain(Species ~ ., data = iris, type = "symuncert")
#'
#' # sparse examples
#' library(Matrix)
#' i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
#' x <- sparseMatrix(i, j, x = x)
#' y = c(1,1,1,1,2,2,2,2)
#'
#' sp_information_gain(x,y)
#' sp_information_gain(x,y, type = "gainratio")
#' sp_information_gain(x,y, type = "symuncert")
#'
#' @importFrom Rcpp evalCpp
#' @importFrom stats na.omit
#' @useDynLib FSelectorRcpp
#' @name FSelectorRcpp
#' @rdname information_gain
#' @export
information_gain = function(formula, data, type = c("infogain", "gainratio", "symuncert"), threads = 1)
{
  type = match.arg(type)

  if(anyNA(data))
  {
    warning("There are missing values in your data. information_gain will remove them with na.omit().")
    data = na.omit(data)
  }

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

  values = information_gain_cpp(data[formula$x], y, threads = threads)
  classEntropy = fs_entropy1d(y)

  results = information_type(classEntropy, values, type)

  data.frame(importance = results, row.names = formula$x)
}



#' @export
#' @rdname information_gain
#' @aliases infotmation_gain
sp_information_gain = function(x, y, type = c("infogain", "gainratio", "symuncert"))
{
  type = match.arg(type)

  values = sparse_information_gain_cpp(x,y)
  classEntropy = fs_entropy1d(y)

  results = information_type(classEntropy, values, type)

  data.frame(importance = results, row.names = colnames(x))
}


information_type = function(classEntropy,
                            values,
                            type = c("infogain", "gainratio", "symuncert"))
{
  attrEntropy  = values$entropy
  jointEntropy = values$joint

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
