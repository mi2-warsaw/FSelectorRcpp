#ifndef FSELECTOR_DISCRETIZE_MDL_H
#define FSELECTOR_DISCRETIZE_MDL_H

#include "FSelectorConfig.h"
#include "entropy/entropy.h"
#include "support/support.h"
#include "boost/optional.hpp"

namespace fselector
{

namespace discretize
{

namespace mdl
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




template<class InputIterator, class VariableIterator, class OutputIterator> std::vector<typename std::iterator_traits<InputIterator>::value_type> discretize(InputIterator itX,
                                                                                                                                                             InputIterator itXLast,
                                                                                                                                                             VariableIterator itY,
                                                                                                                                                             OutputIterator itResult)
{

  std::vector<std::size_t> orderedIdx = support::order(itX, itXLast);

  std::vector<typename std::iterator_traits<InputIterator>::value_type> x;
  std::vector<typename std::iterator_traits<VariableIterator>::value_type> y;


  x.reserve(itXLast - itX);
  y.reserve(itXLast - itX);

  size_t naCounter = 0;

  for(const auto& iter : orderedIdx)
  {
    if(std::isnan(*(itX + iter)))
    {
      naCounter++;
    }

    x.push_back(*(itX + iter));
    y.push_back(*(itY + iter));
  }

  const size_t nonNanCounter = x.size() - naCounter;

  auto xEnd = x.begin() + nonNanCounter;
  auto yEnd = y.begin() + nonNanCounter;

  std::set<int> splitPoints;
  part(x.begin(), xEnd, y.begin(), yEnd, 0, 1.0, splitPoints);

  std::vector<typename std::iterator_traits<InputIterator>::value_type> splitValues;

  for(const auto& sp : splitPoints)
  {
    splitValues.push_back((x[sp] + x[sp + 1])/2);

    for(size_t i  = 0; i < orderedIdx.size(); i++)
    {
      if(std::isnan(*(itX+i)))
      {
        *(itResult + i) = *(itX+i);
      }
      else if(*(itX+i) > x[sp])
      {
        *(itResult + i) += 1;
      }
    }
  }

  return splitValues;

}

} // mdl namespace



} // namespace


}


#endif
