#ifndef FSELECTOR_TABLE_H
#define FSELECTOR_TABLE_H

#include <vector>
#include <map>
#include <utility>

namespace fselector
{

namespace support
{

template<class InputIterator> std::map<typename std::iterator_traits<InputIterator>::value_type, int>
table1d(InputIterator first, InputIterator last)
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

template<class InputIterator, class SecondIterator>
std::map<std::pair<typename std::iterator_traits<InputIterator>::value_type,
                   typename std::iterator_traits<SecondIterator>::value_type>, int>
table2d(InputIterator first, InputIterator last, SecondIterator y)
{
  std::map<std::pair<typename std::iterator_traits<InputIterator>::value_type,
                               typename std::iterator_traits<SecondIterator>::value_type>, int> mapRes;

  for(; first != last; first++, y++)
  {
    const auto value = std::make_pair(*first, *y);
    const auto mit = mapRes.find(value);
    if(mit != mapRes.end())
    {
      mit->second++;
    }
    else
    {
      mapRes[value] = 1;
    }
  }

  return mapRes;
}



}

}

#endif
