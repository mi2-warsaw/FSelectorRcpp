#include <Rcpp.h>
#include "discretize/discretize.h"
#include <string>

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fs_cut_index(const NumericVector& x, const IntegerVector& y)
{

  fselector::discretize::OptPair result = fselector::discretize::cut_index(x.begin(), x.end(), y.begin(), y.end());
  NumericVector res(2);

  if(result)
  {
    res[0] = result->first + 1;
    res[1] = result->second;
  }

  return res;
}

// [[Rcpp::export]]
double fs_mdl_stop(const int& ci, const IntegerVector& y, const double& entropy)
{
  fselector::discretize::OptDouble res = fselector::discretize::mdl_stop(y.begin(), y.end(), ci, entropy);

  if(res) return *res;
  return 0.0;
}

// [[Rcpp::export]]
NumericVector fs_part(const NumericVector& x, const IntegerVector& y)
{
  std::set<int> splitPoints;

  fselector::discretize::part(x.begin(), x.end(), y.begin(), y.end(), 0, 1.0, splitPoints);

  return wrap(splitPoints);

}

// [[Rcpp::export]]
IntegerVector discretize_cpp(const NumericVector& x, const IntegerVector& y)
{
  IntegerVector result(y.size());

  auto splitPoints = fselector::discretize::discretize(x.begin(),
                                    x.end(),
                                    y.begin(),
                                    result.begin());
  result = result + 1; //discretize returns values stratring from zero

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
