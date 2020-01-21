// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "FSelector.h"
#include "support/table.h"
#include "discretize/discretize.h"

using namespace Rcpp;

template<class T1, class T2> void get_entr(double& entr, double& joint, const T1& x, const T2& y) {
  entr = fselector::entropy::entropy1d(x.begin(), x.end());
  auto map = fselector::support::table2d(x.begin(), x.end(), y.begin());
  joint = fselector::entropy::freq_entropy(map.begin(), map.end());
}

// [[Rcpp::export]]
List information_gain_cpp(List xx, IntegerVector y, bool discIntegers, int threads = 1)
{
  IntegerVector result;

  std::vector<double> varEntropy(xx.length());
  std::vector<double> jointEntropy(xx.length());

  std::shared_ptr<fselector::discretize::DiscControl> control =
    std::make_shared<fselector::discretize::mdl::DiscControlMdl>();

  for(int i = 0; i < xx.size(); i++)
  {
    SEXP x = xx[i];

    double entr = 0.0;
    double joint = 0.0;


    switch(TYPEOF(x))
    {
      case REALSXP:
      {
        NumericVector xx = as<NumericVector>(x);
        std::vector<int> disX(y.size());
        fselector::discretize::discretize(xx.begin(), xx.end(), y.begin(), disX.begin(), control);
        get_entr(entr, joint, disX, y);
        break;
      }

      case STRSXP:
      {
        CharacterVector xx = as<CharacterVector>(x);
        get_entr(entr, joint, xx, y);
        break;
      }

      case INTSXP:
      {
        IntegerVector xx = as<IntegerVector>(x);

        if(discIntegers && (!Rf_inherits(x, "factor"))) {
          std::vector<int> disX(y.size());
          std::vector<double> xdouble(xx.begin(), xx.end());
          fselector::discretize::discretize(
            xdouble.begin(), xdouble.end(), y.begin(), disX.begin(), control);
          get_entr(entr, joint, disX, y);

        } else {
          get_entr(entr, joint, xx, y);
        }
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
List sparse_information_gain_cpp(arma::sp_mat x, IntegerVector y, bool discIntegers)
{

  IntegerVector result;

  size_t n = x.n_cols;
  NumericVector varEntropy(n);
  NumericVector jointEntropy(n);

  std::shared_ptr<fselector::discretize::DiscControl> control =
    std::make_shared<fselector::discretize::mdl::DiscControlMdl>();

  for(size_t i =0 ; i < n; i++)
  {
    const auto sp = x.col(i);

    arma::vec xx(sp);

    if(discIntegers) {
      IntegerVector disX(y.size()); //discretized x
      fselector::discretize::discretize(
        xx.begin(), xx.end(), y.begin(), disX.begin(), control);

      varEntropy[i] = fselector::entropy::entropy1d(disX.begin(), disX.end());
      auto map = fselector::support::table2d(disX.begin(), disX.end(), y.begin());
      jointEntropy[i] = fselector::entropy::freq_entropy(map.begin(), map.end());

    } else {

      varEntropy[i] = fselector::entropy::entropy1d(xx.begin(), xx.end());
      auto map = fselector::support::table2d(xx.begin(), xx.end(), y.begin());
      jointEntropy[i] = fselector::entropy::freq_entropy(map.begin(), map.end());
    }

  }


  return Rcpp::List::create(Rcpp::Named("entropy") = varEntropy,
                            Rcpp::Named("joint") = jointEntropy);
}

