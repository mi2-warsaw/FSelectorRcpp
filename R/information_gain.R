information_gain = function(formula, data)
{
  #data = iris
  #formula = Species ~ .
  #information.gain(Species ~ ., data = iris)
  formula = formula2names(formula,data)
  values = information_gain_cpp(data[formula$x], data[[formula$y]])

  classEntropy = fs_entropy1d(data[[formula$y]])

  attrEntropy  = values$entropy
  jointEntropy = values$joint

  results = classEntropy + attrEntropy - jointEntropy

  data.frame(importance = results, row.names = formula$x)
}
