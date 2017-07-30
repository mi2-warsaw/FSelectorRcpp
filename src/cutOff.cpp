#include <Rcpp.h>
#include "cutoff/cutOff.h"
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]
//' Selection Cutoffs for Ranked Attributes
//'
//' @param x1 Vector with attributes' names.
//' @param x2 Vector with attributes' ranks for cut off.
//' @param k A numeric. For \code{k >= 1} it indicates how many attributes to take
//' with the highest attribute rank (chooses k best attributes). For \code{0 < k < 1} it stands for the fraction
//' of top attributes to take (chooses best k * 100% of attributes).
//'
//' @author Damian Skrzypiec, \email{damian.j.skrzypiec@@gmail.com}
//'
//' @examples
//'
//' x <- information_gain(Species ~ ., iris)
//' cutOff_k(x[[1]], x[[2]], 2)
//'
//' @noRd
// [[Rcpp::export]]
std::vector< std::string > cutOff_k(std::vector< std::string >& x1, std::vector<double>& x2, double k)
{
	return(fselector::cutoff::cutOff_k(x1, x2, k, true));
}
