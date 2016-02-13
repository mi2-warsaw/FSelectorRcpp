#ifndef FSELECTOR_SUPPORT_H
#define FSELECTOR_SUPPORT_H

#include <set>
#include <vector>

namespace fselector
{

namespace support
{


template<class InputIterator>
size_t count_levels(InputIterator first, InputIterator last)
{
  std::set<typename std::iterator_traits<InputIterator>::value_type> set(first, last);
  return set.size();
}

template <class InputIterator>
std::vector<size_t> order(InputIterator first, InputIterator last) {
  std::vector<size_t> indices(last - first);
  std::iota(indices.begin(), indices.end(), static_cast<size_t>(0));

  std::sort(
    begin(indices), end(indices),
    [&](size_t a, size_t b) { return *(first + a) < *(first + b); }
  );

  return indices;
}

}

}


#endif
