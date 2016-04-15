##############################################
#
# Damian Skrzypiec
# Test of function cutOff_k()
#
##############################################


# Loading library and function
library(Rcpp)
library(RcppArmadillo)
library(FSelector)

Rcpp::sourceCpp(paste0(getwd(), "/src/cutOff.cpp"))


# Prepare datasets
testDataFrame <- data.frame(x = rnorm(1000000))
rownames(testDataFrame) <- paste0("row_",  1:1000000)


# Testing C++ version:
system.time(x <- cutOff_k(rownames(testDataFrame), testDataFrame$x, k = 1000))

# Testing regular FSelector
system.time(x2 <- FSelector::cutoff.k(testDataFrame, 1000))
