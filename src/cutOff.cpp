#include <Rcpp.h>
#include "cutOff/cutOff.h"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
SEXP cutOff_k(CharacterVector x1, NumericVector x2, SEXP k)
{
	// Transforme variable
	std::vector<std::string> xx = Rcpp::as< std::vector<std::string> >(x1);
	std::vector<double> yy = Rcpp::as< std::vector<double> >(x2);
	double kk = Rcpp::as<double>(k);

	// call the function
	std::vector< std::pair<std::string, double> > zzz = fselector::cutoff::cutOff_k(xx, yy, kk, true);

	// output
	std::vector<double> valueRes;
	std::vector< std::string > names;

	for (std::vector< std::pair<std::string, double> >::iterator i = zzz.begin(); i != zzz.end(); i++)
	{
		names.push_back((*i).first);
		valueRes.push_back((*i).second);
	}

	return Rcpp::DataFrame::create(Rcpp::Named("Attributes") = Rcpp::wrap(names), 
								   Rcpp::Named("Values") = Rcpp::wrap(valueRes)
								   );
}
