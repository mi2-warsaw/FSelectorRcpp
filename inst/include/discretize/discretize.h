#ifndef FSELECTOR_DISCRETIZE_H
#define FSELECTOR_DISCRETIZE_H

#include <vector>
#include <set>
#include <cmath>
#include "entropy/entropy.h"
#include "support/support.h"
#include "boost/optional.hpp"

typedef boost::optional<double> OptDouble;

namespace fselector
{

namespace support
{

template<class InputIterator> OptDouble mdl_stop(InputIterator first, InputIterator last, size_t splitPoint, double gain, double setEntropy)
{
  double k = fselector::support::count_levels(first, last);
  double leftK = fselector::support::count_levels(first, first + splitPoint);
  double rightK = fselector::support::count_levels(first + splitPoint + 1, last);

  double leftEntropy = fselector::entropy::entropy1d(first, first + splitPoint);
  double rightEntropy = fselector::entropy::entropy1d(first + splitPoint + 1, last);

  double delta = std::log(std::pow(3.0, k) - 2) - (k * setEntropy - leftK * leftEntropy - rightK * rightEntropy);

  double len = last - first;
  double condition = (std::log(len - 1.0) + delta) / len;

  OptDouble result;
  if(gain < condition)
    return result;
  else
    result = gain;

  return result;
}

}

}


#endif
