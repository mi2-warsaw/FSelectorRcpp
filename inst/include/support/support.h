#ifndef FSELECTOR_SUPPORT_H
#define FSELECTOR_SUPPORT_H

#include <set>

namespace fselector
{

namespace support
{


template<class InputIterator> size_t count_levels(InputIterator first, InputIterator last, size_t splitPoint, double gain)
{
  std::set<typename std::iterator_traits<InputIterator>::value_type> set;

  for(;first != last; first++)
  {
    set.insert(*first);
  }

  return set.size();

}

}

}


#endif
