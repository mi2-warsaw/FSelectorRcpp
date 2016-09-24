#include "discretizeControl.h"

// [[Rcpp::plugins(cpp11)]]

DiscControl control_builder(Rcpp::List params)
{
  std::string methodChar = params["method"];
  DISCRETIZE_METHOD method = string2discretizeMethod(methodChar);

  DiscControl result(method);
  return result;
}
