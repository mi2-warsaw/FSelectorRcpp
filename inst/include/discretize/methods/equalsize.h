#ifndef FSELECTOR_DISCRETIZE_EQUALSIZE_H
#define FSELECTOR_DISCRETIZE_EQUALSIZE_H

#include "FSelectorConfig.h"
#include "discretize/discretizeControl.h"

namespace fselector
{

namespace discretize
{

namespace equalsize
{

/////////////Control //////////
class DiscControlEqualSize : public DiscControl {
  size_t k_;

public:
  DiscControlEqualSize(DISCRETIZE_METHOD method = DISCRETIZE_METHOD::EQUAL_SIZE, int k = 10) : DiscControl(method), k_(k) {}

  void set_k(size_t k) { k_ = k; }
  size_t get_k() const { return k_; }
};





template<class InputIterator, class VariableIterator, class OutputIterator> std::vector<typename std::iterator_traits<InputIterator>::value_type> discretize(InputIterator itX,
                                                                                                                                                             InputIterator itXLast,
                                                                                                                                                             VariableIterator itY,
                                                                                                                                                             OutputIterator itResult,
                                                                                                                                                             const DiscControlEqualSize& control)
{
  const int k =  control.get_k();
  const std::vector<std::size_t> orderedIdx = support::order(itX, itXLast);

  std::vector<typename std::iterator_traits<InputIterator>::value_type> splitValues;


  std::vector<int> binSizes;

  const long n = orderedIdx.size();
  const long binSize = n / k;
  long rest = n % k;
  long usedRest = 0;

  for(int i = 1; i <= k; i++)
  {
    int tmp = binSize * i + usedRest;
    if(rest > 0) {
      rest--;
      usedRest++;
      tmp++;
    }

    binSizes.push_back(tmp);
  }

  size_t gr = 0;
  for(size_t i = 0; i < orderedIdx.size(); i++)
  {
    if(i >= binSizes[gr]) {
      gr++;
      splitValues.push_back((*(itX + orderedIdx[i-1]) + *(itX + orderedIdx[i])) / 2.0 );
    }

    *(itResult + orderedIdx[i]) = gr;
  }

  return splitValues;

}

} // mdl namespace



} // namespace


}


#endif
