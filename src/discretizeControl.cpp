#include "discretizeControl.h"

// [[Rcpp::plugins(cpp11)]]

DiscControlPair control_builder(Rcpp::List params)
{
  std::string methodChar = params["method"];
  DISCRETIZE_METHOD method = string2discretizeMethod(methodChar);

  DiscControlPair result;
  result.first = method;
  return result;
}
