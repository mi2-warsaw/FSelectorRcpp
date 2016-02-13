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
                                                 double entropy
                                                 )
{
  double k = fselector::support::count_levels(first, last);
  double leftK = fselector::support::count_levels(first, first + splitPoint);
  double rightK = fselector::support::count_levels(first + splitPoint + 1, last);

  double leftEntropy = fselector::entropy::entropy1d(first, first + splitPoint);
  double rightEntropy = fselector::entropy::entropy1d(first + splitPoint + 1, last);
  double totalEntropy = fselector::entropy::entropy1d(first, last);

  double gain = totalEntropy - entropy;

  double delta = std::log(std::pow(3.0, k) - 2) - (k * totalEntropy - leftK * leftEntropy - rightK * rightEntropy);

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


  typedef fselector::entropy::RollEntropy<typename std::iterator_traits<InputIterator>::value_type> RollEntr;

  RollEntr lowerEntropy;
  RollEntr upperEntropy(itY, itYLast);

  for(size_t i = 0; i < lenN - 1; i++)
  {
    lowerEntropy.add_sample(*(itY + i));
    upperEntropy.remove_sample(*(itY + i));

    if(*(itX + i) != *(itX + i + 1))
    {
      const double wn = double(i + 1)/double(lenN);
      const double e1 = wn * lowerEntropy.get_entropy();
      const double e2 = (1.0 - wn) * upperEntropy.get_entropy();
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


template<class InputIterator, class OutputIterator> OptPair gr(InputIterator itX,
                                                                InputIterator itXLast,
                                                                OutputIterator itY,
                                                                OutputIterator itYLast,
                                                                double depth

)
{
  OptPair result;

  OptPair ct = cut_index(itX, itXLast, itY, itYLast);
  if(!ct) return result;

  OptDouble mdl = mdl_stop(itY, itYLast, ct->first, ct->second);

  if(!mdl) return result;

  result = std::make_pair(ct->first, depth + 1);
  return result;

}

template<class InputIterator, class OutputIterator> void part(InputIterator itX,
                                                               InputIterator itXLast,
                                                               OutputIterator itY,
                                                               OutputIterator itYLast,
                                                               int low,
                                                               double depth,
                                                               std::set<int>& splitPoints

)
{
  if(itXLast - itX < 2) return;

  OptPair cc = gr(itX, itXLast, itY, itYLast, depth);
  if(!cc) return;

  depth = cc->second;
  int ci = cc->first + 1;

  splitPoints.insert(low + ci - 1);

  part(itX, itX + ci, itY, itY + ci, low, depth, splitPoints);
  part(itX + ci + 1, itXLast, itY + ci + 1, itYLast, low + ci + 1, depth, splitPoints);
}


template<class InputIterator, class OutputIterator> void discretize(InputIterator itX,
                InputIterator itXLast,
                OutputIterator itY,
                OutputIterator itResult)
{

  std::vector<std::size_t> orderedIdx = support::order(itX, itXLast);

  std::vector<typename std::iterator_traits<InputIterator>::value_type> x;
  std::vector<typename std::iterator_traits<OutputIterator>::value_type> y;

  for(const auto& iter : orderedIdx)
  {
    x.push_back(*(itX + iter));
    y.push_back(*(itY + iter));
  }

  std::set<int> splitPoints;
  part(x.begin(), x.end(), y.begin(), y.end(), 0, 1.0, splitPoints);


  for(const auto& sp : splitPoints)
  {
    for(size_t i  = 0; i < orderedIdx.size(); i++)
    {
      if(*(itX+i) > x[sp])
      {
        *(itResult + i) += 1;
      }
    }
  }

}




} // namespace


}


#endif