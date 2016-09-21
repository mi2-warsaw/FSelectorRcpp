#include <Rcpp.h>
#include "support/support.h"
#include "support/table.h"
#include "entropy/entropy.h"
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

  stop("Not supported type.");
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

  stop("Not supported type.");
}

////////////////////////// Entropy //////////////////////

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

  stop("Not supported type.");
}

//////////////////////////// Table /////////////////////////////

template<typename T> IntegerVector fs_map2table(const std::map<T, int>& map)
{
  size_t n = map.size();
  IntegerVector res(n);
  CharacterVector names(n);

  auto iter = map.begin();
  for(size_t i = 0; i < n; i++)
  {
    res[i] = iter->second;
    names[i] = iter->first;

    iter++;
  }

  res.attr("names") = names;

  return res;
}

// [[Rcpp::export]]
IntegerVector fs_table1d(SEXP& x)
{
  IntegerVector result;

  switch(TYPEOF(x))
  {
  case REALSXP:
  {
    NumericVector xx = as<NumericVector>(x);
    auto res = fselector::support::table1d(xx.begin(), xx.end());
    result = fs_map2table(res);
    break;
  }

  case STRSXP:
  {
    CharacterVector xx = as<CharacterVector>(x);
    auto res = fselector::support::table1d(xx.begin(), xx.end());
    result = fs_map2table(res);
    break;
  }

  case INTSXP:
  {
    IntegerVector xx = as<IntegerVector>(x);
    auto res = fselector::support::table1d(xx.begin(), xx.end());
    result = fs_map2table(res);


    if(xx.hasAttribute("class"))
    {
      if(std::strncmp(xx.attr("class"), "factor", 6) == 0)
      {
        result.attr("names") = xx.attr("levels");
      }
    }

    break;
  }

  }


  return  result;

}

// [[Rcpp::export]]
std::vector<int> fs_table_numeric2d(NumericVector &x, NumericVector &y)
{
  std::vector<int> result;

  auto res = fselector::support::table2d(x.begin(), x.end(), y.begin());

  for(const auto& it : res)
  {
    result.push_back(it.second);
  }

  return  result;

}




