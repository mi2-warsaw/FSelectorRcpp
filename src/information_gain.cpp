// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "FSelector.h"
#include "support/table.h"
#include "discretize/discretize.h"

using namespace Rcpp;

// [[Rcpp::export]]
List information_gain_cpp(List xx, IntegerVector y, int threads = 1)
{

  IntegerVector result;

  NumericVector varEntropy(xx.length());
  NumericVector jointEntropy(xx.length());

  if(threads < 1) threads = omp_get_max_threads();

  #pragma omp parallel for shared(xx, varEntropy, jointEntropy, y) num_threads(threads) schedule(dynamic)
  for(size_t i = 0; i < xx.size(); i++)
  {
    SEXP x = xx[i];

    double entr = 0.0;
    double joint = 0.0;


    switch(TYPEOF(x))
    {
      case REALSXP:
      {
        NumericVector xx = as<NumericVector>(x);

        IntegerVector disX(y.size()); //discretized x
        fselector::discretize::discretize(xx.begin(), xx.end(), y.begin(), disX.begin());

        entr = fselector::entropy::entropy1d(disX.begin(), disX.end());

        auto map = fselector::support::table2d(disX.begin(),disX.end(), y.begin());
        joint = fselector::entropy::freq_entropy(map.begin(), map.end());

        break;
      }

      case STRSXP:
      {
        CharacterVector xx = as<CharacterVector>(x);
        entr = fselector::entropy::entropy1d(xx.begin(), xx.end());

        auto map = fselector::support::table2d(xx.begin(), xx.end(), y.begin());
        joint = fselector::entropy::freq_entropy(map.begin(), map.end());
        break;
      }

      case INTSXP:
      {
        IntegerVector xx = as<IntegerVector>(x);

        entr = fselector::entropy::entropy1d(xx.begin(), xx.end());

        auto map = fselector::support::table2d(xx.begin(), xx.end(), y.begin());
        joint = fselector::entropy::freq_entropy(map.begin(), map.end());
        break;
      }
    }

    varEntropy[i] = entr;
    jointEntropy[i] = joint;
  }

  return Rcpp::List::create(Rcpp::Named("entropy") = varEntropy,
                            Rcpp::Named("joint") = jointEntropy);
}



// [[Rcpp::export]]
List sparse_information_gain_cpp(arma::sp_mat x, IntegerVector y)
{

  IntegerVector result;

  size_t n = x.n_cols;
  NumericVector varEntropy(n);
  NumericVector jointEntropy(n);

  for(size_t i =0 ; i < n; i++)
  {
    const auto sp = x.col(i);

    arma::vec xx(sp);

    //IntegerVector disX(y.size()); //discretized x
    //fselector::discretize::discretize(xx.begin(), xx.end(), y.begin(), disX.begin());

    varEntropy[i] = fselector::entropy::entropy1d(xx.begin(), xx.end());

    auto map = fselector::support::table2d(xx.begin(), xx.end(), y.begin());

    jointEntropy[i] = fselector::entropy::freq_entropy(map.begin(), map.end());
  }


  return Rcpp::List::create(Rcpp::Named("entropy") = varEntropy,
                            Rcpp::Named("joint") = jointEntropy);
}

