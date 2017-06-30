library(testthat)
library(FSelectorRcpp)
library(FSelector)
library(Matrix)
library(Rcpp)
library(RcppArmadillo)
library(dplyr)
library(entropy)
library(lintr)

test_check("FSelectorRcpp")

linters <- default_linters
linters$camel_case_linter <- NULL
expect_lint_free(linters = linters)
