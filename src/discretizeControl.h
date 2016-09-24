#include <Rcpp.h>
#include "discretize/discretize.h"
#include <string>
#include <utility>

using namespace Rcpp;
using namespace fselector::discretize;

DiscControl control_builder(Rcpp::List params);

