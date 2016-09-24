#include <Rcpp.h>
#include "discretize/discretize.h"
#include <string>
#include <utility>

using namespace Rcpp;
using namespace fselector::discretize;

typedef std::pair<DISCRETIZE_METHOD, DiscControl> DiscControlPair;

DiscControlPair control_builder(Rcpp::List params);

