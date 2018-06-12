#include <Rcpp.h>
#include <testthat.h>

#include "support/support.h"
#include "support/table.h"
#include "entropy/entropy.h"
#include <cmath>

bool is_eps_equal(double x, double y) {
  return std::abs(x - y) < 0.00001;
}

context("Entropy tests") {

  test_that("Basic test") {

    std::vector<int> x = {1, 1, 2};
    double entr = fselector::entropy::entropy1d(x.begin(), x.end());

    expect_true(is_eps_equal(entr, 0.6365142));
  }

  test_that("Numeric entropy") {

    std::vector<int> x = {1, 1, 2};
    double entr = fselector::entropy::numeric_entropy(x.begin(), x.end());

    expect_true(is_eps_equal(entr, 1.039721));
  }

  test_that("Numeric entropy - comparsion with entropy package") {
    Rcpp::Environment entrEnv("package:entropy");
    Rcpp::Function    entropyFnc = entrEnv["entropy"];

    Rcpp::NumericVector x     = {1.0,2.0,3.0};
    Rcpp::NumericVector entrR = entropyFnc(x);

    expect_true(
      is_eps_equal(
        entrR[0], fselector::entropy::numeric_entropy(x.begin(), x.end()
    )));
  }

  test_that("Table entropy - comparsion with entropy package") {
    Rcpp::Environment entrEnv("package:entropy");
    Rcpp::Function    entropyFnc = entrEnv["entropy"];

    Rcpp::NumericVector x     = {1,2,3};
    Rcpp::NumericVector entrR = entropyFnc(x);

    std::vector<int> xx = {1,2,2,3,3,3};

    expect_true(
      is_eps_equal(
        entrR[0], fselector::entropy::entropy1d(xx.begin(), xx.end())));
  }

}

context("Other tests") {

  test_that("Count levels") {
    std::vector<int> xx = {1,2,2,3,3,3};

    expect_true(3 == fselector::support::count_levels(xx.begin(), xx.end()));
  }

}
