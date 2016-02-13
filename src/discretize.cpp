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


/*** R

*/
