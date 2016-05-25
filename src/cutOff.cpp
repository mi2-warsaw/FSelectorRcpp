#include <Rcpp.h>
#include "cutoff/cutOff.h"
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]
//' Selection Cutoffs for Ranked Attributes
//'
//' @param x1 Probably vector with attributes' names.
//' @param x2 Probably vector with attributes' ranks fo cut off.
//' @param k A numeric. For \code{k > 1} it indicates how many attributes to take
//' with the highest attribute rank (chooses k best attributes). For \code{0 < k  <=1} it stands for the percent
//' of top attributes to take (chooses best k * 100% of attributes). For \code{k = 0} it selects top attributes
//' before the biggest gap in differences of scores for neighbouring attributes
//' (chooses a subset of attributes which are significantly better than other.)
//'
//' @author Damian Skrzypiec, \email{damian.j.skrzypiec@@gmail.com}
//'
//' @examples
//'
//' x = information_gain(Species ~ ., iris)
//' cutOff_k(rownames(x), x[[1]], 2)
//'
//' @export
// [[Rcpp::export]]
std::vector< std::string > cutOff_k(std::vector< std::string >& x1, std::vector<double>& x2, double k)
{
	return(fselector::cutoff::cutOff_k(x1, x2, k, true));
}
