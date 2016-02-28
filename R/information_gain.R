information_gain = function(formula, data)
{
  formula = formula2names(formula,data)

  y = data[[formula$y]]

  if(anyNA())
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

  data.frame(importance = results, row.names = formula$x)
}
