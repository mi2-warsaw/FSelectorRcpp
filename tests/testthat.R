library(testthat)
library(FSelectorRcpp)
library(Matrix)
library(Rcpp)
library(RcppArmadillo)
library(dplyr)
library(entropy)
library(lintr)

iris_plus <- setNames(iris, gsub(
  pattern = "\\.",
  replacement = "+",
  x = colnames(iris)
))

test_check("FSelectorRcpp")
