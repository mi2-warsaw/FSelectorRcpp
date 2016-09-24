#ifndef FSELECTOR_DISCRETIZE_H
#define FSELECTOR_DISCRETIZE_H

#include "discretize/methods/mdl.h"
#include "discretize/methods/equalsize.h"

namespace fselector
{

namespace discretize
{

template<class InputIterator,
         class VariableIterator,
         class OutputIterator>
         std::vector<typename std::iterator_traits<InputIterator>::value_type> discretize(InputIterator itX,
                                                                                          InputIterator itXLast,
                                                                                          VariableIterator itY,
                                                                                          OutputIterator itResult,
                                                                                          std::shared_ptr<DiscControl> control)
{
  DISCRETIZE_METHOD method = control->get_method();

  switch(method)
  {
    case DISCRETIZE_METHOD::MDL:
      return mdl::discretize(itX, itXLast, itY, itResult);
      break;

    case DISCRETIZE_METHOD::EQUAL_SIZE:
      std::shared_ptr<equalsize::DiscControlEqualSize> controlEqual = std::static_pointer_cast<equalsize::DiscControlEqualSize>(control);
      return equalsize::discretize(itX, itXLast, itY, itResult, *controlEqual);
      break;
  }


}


}
}

#endif
