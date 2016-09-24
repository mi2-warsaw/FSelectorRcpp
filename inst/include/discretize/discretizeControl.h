#ifndef FSELECTOR_DISCRETIZE_CONTROL_H
#define FSELECTOR_DISCRETIZE_CONTROL_H

#include <memory>

namespace fselector
{

namespace discretize
{

enum DISCRETIZE_METHOD {
  MDL = 0,
  EQUAL_SIZE = 1
};

template<class STRING> DISCRETIZE_METHOD string2discretizeMethod(const STRING& method)
{
  if("MDL" == method) return DISCRETIZE_METHOD::MDL;
  if("EQUAL_SIZE" == method) return DISCRETIZE_METHOD::EQUAL_SIZE;
  return DISCRETIZE_METHOD::MDL;
}

////////////////////////////////////////////////////////////////////////////////
class DiscControl {
  DISCRETIZE_METHOD method_;

public:
  DiscControl(DISCRETIZE_METHOD method = DISCRETIZE_METHOD::MDL) : method_(method) {}
  DISCRETIZE_METHOD get_method() const { return method_; }
};



}
}

#endif
