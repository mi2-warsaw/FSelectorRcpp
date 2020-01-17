// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include "FSelector.h"
#include "support/table.h"
#include "discretize/discretize.h"
#include "boost/variant.hpp"

using namespace Rcpp;

template<class T1, class T2> void get_entr(double& entr, double& joint, const T1& x, const T2& y) {
  entr = fselector::entropy::entropy1d(x.begin(), x.end());
  auto map = fselector::support::table2d(x.begin(), x.end(), y.begin());
  joint = fselector::entropy::freq_entropy(map.begin(), map.end());
}


class BaseWrp {
  public:
    virtual ~BaseWrp() = default;
};

template<class Y>
class Wrp : public BaseWrp
{
  public:
  Y ptr;

  Wrp(Y x): ptr(x) {}
};

//// https://gallery.rcpp.org/articles/fast-factor-generation/
template <int RTYPE>
IntegerVector fast_factor_template( const Vector<RTYPE>& x ) {
  Vector<RTYPE> levs = sort_unique(x);
  IntegerVector out = match(x, levs);
  out.attr("levels") = as<CharacterVector>(levs);
  out.attr("class") = "factor";
  return out;
}

SEXP fast_factor( SEXP x ) {
  switch( TYPEOF(x) ) {
  case INTSXP: return fast_factor_template<INTSXP>(x);
  case REALSXP: return fast_factor_template<REALSXP>(x);
  case STRSXP: return fast_factor_template<STRSXP>(x);
  }
  return R_NilValue;
}

// [[Rcpp::export]]
List information_gain_cpp(List xx, IntegerVector y, bool discIntegers, int threads = 1)
{
  RcppParallel::RVector<int> yy(y);
  IntegerVector result;

  std::vector<double> varEntropy(xx.length());
  std::vector<double> jointEntropy(xx.length());

  if(threads < 1) threads = omp_get_max_threads();

  std::shared_ptr<fselector::discretize::DiscControl> control =
    std::make_shared<fselector::discretize::mdl::DiscControlMdl>();

  //RcppParallel::RVector<std::string>, RcppParallel::RVector<double>, RcppParallel::RVector<int>>
  std::vector<std::shared_ptr<BaseWrp>> safePtr;

  for(int i = 0; i < xx.size(); i++) {
    SEXP x = xx[i];

    switch(TYPEOF(x))
    {
      case REALSXP:
      {
        //https://en.cppreference.com/w/cpp/memory/shared_ptr/pointer_cast
        RcppParallel::RVector<double> rptr(as<NumericVector>(x));
        std::shared_ptr<BaseWrp> ptr = std::make_shared<Wrp<RcppParallel::RVector<double>>>(rptr);
        safePtr.push_back(ptr);
        break;
      }

      case STRSXP:
      {
        RcppParallel::RVector<int> rptr(as<IntegerVector>(fast_factor(x)));
        std::shared_ptr<BaseWrp> ptr = std::make_shared<Wrp<RcppParallel::RVector<int>>>(rptr);
        safePtr.push_back(ptr);
        break;
      }

      case INTSXP:
      {
        RcppParallel::RVector<int> rptr(as<IntegerVector>(x));
        std::shared_ptr<BaseWrp> ptr = std::make_shared<Wrp<RcppParallel::RVector<int>>>(rptr);
        safePtr.push_back(ptr);
        break;
      }
    }
  }


  #pragma omp parallel for shared(xx, varEntropy, jointEntropy, y, control) num_threads(threads) schedule(dynamic)
  for(int i = 0; i < xx.size(); i++)
  {
    SEXP x = xx[i];

    double entr = 0.0;
    double joint = 0.0;


    switch(TYPEOF(x))
    {
      case REALSXP:
      {
        std::vector<int> disX(y.size());

        std::shared_ptr<Wrp<RcppParallel::RVector<double>>> sp =
          std::dynamic_pointer_cast<Wrp<RcppParallel::RVector<double>>>(safePtr.at(i));

        fselector::discretize::discretize(sp->ptr.begin(), sp->ptr.end(), yy.begin(), disX.begin(), control);
        get_entr(entr, joint, disX, yy);
        break;
      }

      case STRSXP:
      {
        std::shared_ptr<Wrp<RcppParallel::RVector<int>>> sp =
          std::dynamic_pointer_cast<Wrp<RcppParallel::RVector<int>>>(safePtr.at(i));

        RcppParallel::RVector<int> xx = sp->ptr;
        get_entr(entr, joint, xx, y);
        break;
      }

      case INTSXP:
      {
        std::shared_ptr<Wrp<RcppParallel::RVector<int>>> sp =
          std::dynamic_pointer_cast<Wrp<RcppParallel::RVector<int>>>(safePtr.at(i));

        RcppParallel::RVector<int> xx = sp->ptr;

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

