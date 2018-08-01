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

  Rcpp::CharacterVector splitVals(splitPoints.size() + 1);

  size_t i = 0;
  std::string first = "(-Inf;";
  std::string last  = "";
  for(auto iter = splitPoints.begin(); iter != splitPoints.end(); iter++ )
  {
    const std::string spl  = std::to_string(*iter);
    const std::string frmt = first + spl + "]";
    first = std::string("(") + spl + ";";
    splitVals[i] = frmt;
    i++;
  }

  if(splitPoints.empty())
  {
    IntegerVector x(1, IntegerVector::get_na());
    return x;
  }

  splitVals[splitPoints.size()] = std::string("(") +
                                    std::to_string(splitPoints.back()) + ";" + "Inf)";

  result.attr("levels") = splitVals;

  CharacterVector cl(2);
  cl[0] = "ordered";
  cl[1] = "factor";


  Rcpp::NumericVector splitValues(splitPoints.begin(), splitPoints.end());
  splitValues.push_front(R_NegInf);
  splitValues.push_back(R_PosInf);

  //result.attr("class") = cl;
  result.attr("SplitValues") = splitValues;

  return result;

}


/*** R

  */
