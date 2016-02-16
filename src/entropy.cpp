#include <Rcpp.h>
#include "support/table.h"
#include "entropy/entropy.h"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
double fs_entropy1d(SEXP x)
{
  switch(TYPEOF(x))
  {
    case REALSXP:
    {
      NumericVector xx = as<NumericVector>(x);
      return fselector::entropy::entropy1d(xx.begin(), xx.end());
    }

    case STRSXP:
    {
      CharacterVector xx = as<CharacterVector>(x);
      return fselector::entropy::entropy1d(xx.begin(), xx.end());
    }

    case INTSXP:
    {
      IntegerVector xx = as<IntegerVector>(x);
      return fselector::entropy::entropy1d(xx.begin(), xx.end());
    }
  }

  return 0.0;
}

// [[Rcpp::export]]
double fs_numeric_entropy(const NumericVector& x)
{
  return fselector::entropy::numeric_entropy(x.begin(), x.end());
}

