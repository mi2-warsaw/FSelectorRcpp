#include <Rcpp.h>
#include "discretize/discretize.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fs_cut_index(NumericVector x, IntegerVector y)
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
double fs_mdl_stop(int ci, IntegerVector y, double entropy)
{
  fselector::discretize::OptDouble res = fselector::discretize::mdl_stop(y.begin(), y.end(), ci, entropy);

  if(res) return *res;
  return 0.0;
}

// [[Rcpp::export]]
NumericVector fs_part(NumericVector x, IntegerVector y)
{
  std::set<int> splitPoints;

  fselector::discretize::part(x.begin(), x.end(), y.begin(), y.end(), 0, 1.0, splitPoints);

  return wrap(splitPoints);

}

// [[Rcpp::export]]
IntegerVector fs_discretize(NumericVector x, IntegerVector y)
{
  IntegerVector result(y.size());

  fselector::discretize::discretize(x.begin(), x.end(), y.begin(), result.begin());

  return result;

}


/*** R

  */
