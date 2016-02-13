#include <Rcpp.h>
#include "support/support.h"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
int fs_count_levels(SEXP x)
{
  switch(TYPEOF(x))
  {
  case REALSXP:
  {
    NumericVector xx = as<NumericVector>(x);
    return fselector::support::count_levels(xx.begin(), xx.end());
  }

  case STRSXP:
  {
    CharacterVector xx = as<CharacterVector>(x);
    return fselector::support::count_levels(xx.begin(), xx.end());
  }

  case INTSXP:
  {
    IntegerVector xx = as<IntegerVector>(x);
    return fselector::support::count_levels(xx.begin(), xx.end());
  }
  }

  return 0;
}

// [[Rcpp::export]]
IntegerVector fs_order(SEXP x)
{
  switch(TYPEOF(x))
  {
  case REALSXP:
  {
    NumericVector xx = as<NumericVector>(x);
    return wrap(fselector::support::order(xx.begin(), xx.end()));
  }

  case INTSXP:
  {
    IntegerVector xx = as<IntegerVector>(x);
    return wrap(fselector::support::order(xx.begin(), xx.end()));
  }
  }

  IntegerVector xx;
  return xx;
}
