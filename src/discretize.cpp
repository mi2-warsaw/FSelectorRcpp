#include <Rcpp.h>
#include "discretize/discretize.h"
#include <string>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector discretize_cpp(const NumericVector& x, const IntegerVector& y)
{
  IntegerVector result(y.size());

  auto splitPoints = fselector::discretize::discretize(x.begin(),
                                    x.end(),
                                    y.begin(),
                                    result.begin());
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

  splitVals[splitPoints.size()] = std::string("(") +
                                    std::to_string(splitPoints.back()) + ";" + "Inf)";

  result.attr("levels") = splitVals;

  CharacterVector cl(2);
  cl[0] = "ordered";
  cl[1] = "factor";

  result.attr("class") = cl;

  return result;

}


/*** R

  */
