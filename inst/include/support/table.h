#ifndef FSELECTOR_TABLE_H
#define FSELECTOR_TABLE_H

#include <vector>
#include <map>

namespace fselector
{

namespace support
{

template<class InputIterator> std::map<typename std::iterator_traits<InputIterator>::value_type, int> table1d(InputIterator first, InputIterator last)
{
  std::map<typename std::iterator_traits<InputIterator>::value_type, int> mapRes;

  for(auto iter = first; iter != last; iter++)
  {
    auto mit = mapRes.find(*iter);
    if(mit != mapRes.end())
    {
      mit->second++;
    }
    else
    {
      mapRes[*iter] = 1;
    }
  }

  return mapRes;
}

}

}

#endif
