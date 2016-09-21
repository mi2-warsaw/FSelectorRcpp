#include <Rcpp.h>
#include <testthat.h>

#include "support/table.h"
#include "entropy/entropy.h"

context("Entropy tests") {

  test_that("Basic test") {

    std::vector<int> x = {1, 1, 2};
    double entr = fselector::entropy::entropy1d(x.begin(), x.end());

    expect_true(std::fabs(entr - 0.6365142) < 0.00001);
  }

  test_that("Numeric entropy") {

    std::vector<int> x = {1, 1, 2};
    double entr = fselector::entropy::numeric_entropy(x.begin(), x.end());

    expect_true(std::fabs(entr - 1.039721) < 0.00001);
  }

  test_that("Numeric entropy - comparsion with entropy package") {
    Rcpp::Environment entrEnv("package:entropy");
    Rcpp::Function    entropyFnc = entrEnv["entropy"];

    Rcpp::NumericVector x     = {1.0,2.0,3.0};
    Rcpp::NumericVector entrR = entropyFnc(x);

    expect_true(entrR[0] == fselector::entropy::numeric_entropy(x.begin(), x.end()));
  }

}
