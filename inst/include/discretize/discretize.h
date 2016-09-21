#ifndef FSELECTOR_DISCRETIZE_H
#define FSELECTOR_DISCRETIZE_H

#include "discretize/methods/mdl.h"
#include "discretize/methods/equalsize.h"

namespace fselector
{

namespace discretize
{

enum DISCRETIZE_METHOD {
  MDL = 0,
  EQUAL_SIZE = 1
};

class DiscControl {
  size_t k_;

  public:
    DiscControl() : k_(10) {}
    void set_k(size_t k) { k_ = k; }
    size_t get_k() { return k_; }
};

template<class InputIterator,
         class VariableIterator,
         class OutputIterator>
         std::vector<typename std::iterator_traits<InputIterator>::value_type> discretize(InputIterator itX,
                                                                                          InputIterator itXLast,
                                                                                          VariableIterator itY,
                                                                                          OutputIterator itResult,
                                                                                          DiscControl control,
                                                                                          DISCRETIZE_METHOD method = DISCRETIZE_METHOD::MDL)
{
  switch(method)
  {
    case DISCRETIZE_METHOD::MDL:
      return mdl::discretize(itX, itXLast, itY, itResult);
      break;

    case DISCRETIZE_METHOD::EQUAL_SIZE:
      return equalsize::discretize(itX, itXLast, itY, itResult, control.get_k());
      break;
  }


}


}
}

#endif
