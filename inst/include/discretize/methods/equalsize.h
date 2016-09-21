#ifndef FSELECTOR_DISCRETIZE_EQUALSIZE_H
#define FSELECTOR_DISCRETIZE_EQUALSIZE_H

#include "FSelectorConfig.h"

namespace fselector
{

namespace discretize
{

namespace equalsize
{

template<class InputIterator, class VariableIterator, class OutputIterator> std::vector<typename std::iterator_traits<InputIterator>::value_type> discretize(InputIterator itX,
                                                                                                                                                             InputIterator itXLast,
                                                                                                                                                             VariableIterator itY,
                                                                                                                                                             OutputIterator itResult,
                                                                                                                                                             size_t k = 10)
{

  const std::vector<std::size_t> orderedIdx = support::order(itX, itXLast);

  std::vector<typename std::iterator_traits<InputIterator>::value_type> splitValues;

  std::vector<int> binSizes;

  const long n = orderedIdx.size();
  const long binSize = n / k;
  long rest = n % k;

  for(int i = 1; i <= k; i++)
  {
    int tmp = binSize * i;
    if(rest > 0) {
      rest--;
      tmp++;
    }

    binSizes.push_back(tmp);
  }

  size_t gr = 0;
  for(size_t i = 0; i < orderedIdx.size(); i++)
  {
    if(i > binSizes[gr]) {
      gr++;
      splitValues.push_back((*(itX + orderedIdx[i-1]) + *(itX + orderedIdx[i])) / 2.0 );
    }

    *(itResult + orderedIdx[i-1]) = gr;
  }

  return splitValues;

}

} // mdl namespace



} // namespace


}


#endif
