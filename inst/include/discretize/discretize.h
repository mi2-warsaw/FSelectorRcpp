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
  DISCRETIZE_METHOD method_;
  size_t k_;

  public:
    DiscControl(DISCRETIZE_METHOD method = DISCRETIZE_METHOD::MDL) : method_(method), k_(10) {}

    DISCRETIZE_METHOD get_method() const { return method_; }

    void set_k(size_t k) { k_ = k; }
    size_t get_k() const { return k_; }
};

template<class STRING> DISCRETIZE_METHOD string2discretizeMethod(const STRING& method)
{
  if("MDL" == method) return DISCRETIZE_METHOD::MDL;
  if("EQUAL_SIZE" == method) return DISCRETIZE_METHOD::EQUAL_SIZE;
  return DISCRETIZE_METHOD::MDL;
}

template<class InputIterator,
         class VariableIterator,
         class OutputIterator>
         std::vector<typename std::iterator_traits<InputIterator>::value_type> discretize(InputIterator itX,
                                                                                          InputIterator itXLast,
                                                                                          VariableIterator itY,
                                                                                          OutputIterator itResult,
                                                                                          const DiscControl& control)
{
  DISCRETIZE_METHOD method = control.get_method();

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
