#include <Rcpp.h>
#include "cutOff/cutOff.h"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::vector< std::string > cutOff_k(std::vector< std::string >& x1, std::vector<double>& x2, double k)
{
	return(fselector::cutoff::cutOff_k(x1, x2, k, true));
}
