#ifndef FSELECTOR_DISCRETIZE_CONTROL_R_H
#define FSELECTOR_DISCRETIZE_CONTROL_R_H

#include <Rcpp.h>
#include "discretize/discretize.h"
#include <string>
#include <utility>

using namespace Rcpp;
using namespace fselector::discretize;

std::shared_ptr<DiscControl> control_builder(Rcpp::List params);

#endif
