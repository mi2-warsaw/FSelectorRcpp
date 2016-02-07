#include <Rcpp.h>
#include "support/table.h"
#include "entropy/entropy.h"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
double fs_freq_entropy(SEXP x)
{
  switch(TYPEOF(x))
  {
    case REALSXP:
    {
      NumericVector xx = as<NumericVector>(x);
      auto res = fselector::support::table1d(xx.begin(), xx.end());
      return fselector::entropy::freq_entropy(res.begin(), res.end());
    }

    case STRSXP:
    {
      CharacterVector xx = as<CharacterVector>(x);
      auto res = fselector::support::table1d(xx.begin(), xx.end());
      return fselector::entropy::freq_entropy(res.begin(), res.end());

    }

    case INTSXP:
    {
      IntegerVector xx = as<IntegerVector>(x);
      auto res = fselector::support::table1d(xx.begin(), xx.end());
      return fselector::entropy::freq_entropy(res.begin(), res.end());
    }
  }

  return 0.0;
}

// [[Rcpp::export]]
double fs_numeric_entropy(NumericVector x)
{
  return fselector::entropy::numeric_entropy(x.begin(), x.end());
}

