#ifndef FSELECTOR_DISCRETIZE_H
#define FSELECTOR_DISCRETIZE_H

#include <vector>
#include <set>
#include <cmath>
#include <utility>
#include "entropy/entropy.h"
#include "support/support.h"
#include "boost/optional.hpp"
#include <iostream>


namespace fselector
{

namespace discretize
{


typedef boost::optional<double> OptDouble;
typedef boost::optional<std::pair<size_t, double>> OptPair;

template<class InputIterator> OptDouble mdl_stop(InputIterator first,
                                                 InputIterator last,
                                                 size_t splitPoint,
                                                 double gain,
                                                 double setEntropy)
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

template<class InputIterator, class OutputIterator> OptPair cut_index(InputIterator itX,
                                                InputIterator itXLast,
                                                OutputIterator itY,
                                                OutputIterator itYLast
                                                )
{
  OptPair result;

  double entropy = 999999.0;
  int ci = 0;
  bool init = false;

  const size_t lenN = itXLast - itX;

  for(size_t i = 0; i < lenN - 1; i++)
  {
    if(*(itX + i) != *(itX + i + 1))
    {
      const double ct = (*(itX + i) + *(itX + i + 1)) / 2.0;
      const double wn = double(i + 1)/double(lenN);

      const double e1 = wn * fselector::entropy::entropy1d(itY, itY + i + 1);
      const double e2 = (1.0 - wn) * fselector::entropy::entropy1d(itY + i + 1, itYLast);
      const double val = e1 + e2;

      if(val < entropy)
      {
        entropy = val;
        ci = i;
        init = true;
      }
    }
  }

  if(init)
  {
    result = std::make_pair(ci, entropy);
  }

  return result;
}




}


}


#endif
