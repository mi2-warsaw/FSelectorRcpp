#include "discretizeControlR.h"

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
IntegerVector discretize_cpp(const NumericVector& x, const IntegerVector& y, const List& discControl)
{
  IntegerVector result(y.size());

  std::shared_ptr<fselector::discretize::DiscControl> control = control_builder(discControl);

  auto splitPoints = fselector::discretize::discretize(x.begin(),
                                    x.end(),
                                    y.begin(),
                                    result.begin(),
                                    control);
  result = result + 1; //discretize returns values starting from zero

  if(splitPoints.empty())
  {
    IntegerVector x(1, IntegerVector::get_na());
    return x;
  }

  Rcpp::NumericVector splitValues(splitPoints.begin(), splitPoints.end());
  splitValues.push_front(R_NegInf);
  splitValues.push_back(R_PosInf);

  result.attr("SplitValues") = splitValues;

  return result;

}
