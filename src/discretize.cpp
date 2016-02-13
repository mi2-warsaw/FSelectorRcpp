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


/*** R
x = iris$Sepal.Length
y = as.integer(iris$Species[order(x)])
x = sort(x)

ct <- discretization::cutIndex(x, y)


mdlStop(ct[1],y,ct[2])
fs_mdl_stop(ct[1],y,ct[2])

mdlStop(1,y,0.7)
fs_mdl_stop(1,y,0.7)


  */
