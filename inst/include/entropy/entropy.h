#ifndef FSELECTOR_ENTROPY_H
#define FSELECTOR_ENTROPY_H

#include <vector>
#include <map>
#include <cmath>
#include <algorithm>
#include "support/table.h"

namespace fselector
{

namespace entropy
{


template<class InputIterator> double freq_entropy(InputIterator first, InputIterator last)
{

  const double sum = std::accumulate(first, last, 0.0,
                              [](const double& a, typename std::iterator_traits<InputIterator>::value_type iter)
                              {
                                return a + iter.second;
                              });

  const double result = std::accumulate(first, last, 0.0, [sum](const double& a, typename std::iterator_traits<InputIterator>::value_type iter)
                            {
                              if(iter.second > 0)
                              {
                                const double res = iter.second/sum;
                                return a + res * std::log(res);
                              } else
                              {
                                return a;
                              }

                            });



  return -result;
}

//// numeric entropy
template<class InputIterator> double numeric_entropy(InputIterator first, InputIterator last)
{

  const double sum = std::accumulate(first, last, 0.0);

  const double result = std::accumulate(first, last, 0.0, [sum](const double& a, typename std::iterator_traits<InputIterator>::value_type iter)
  {
    if(iter > 0)
    {
      const double res = iter/sum;
      return a + res * std::log(res);
    } else
    {
      return a;
    }

  });



  return -result;
}

template<class InputIterator> double entropy1d(InputIterator first, InputIterator last)
{
  const auto table = fselector::support::table1d(first, last);
  return freq_entropy(table.begin(), table.end());
}

template<typename T> class RollEntropy
{
  std::map<T, int> _map;
  int _size;

  public:

    RollEntropy() : _size(0) {};
    template<class InputIterator> RollEntropy(InputIterator first, InputIterator last) :
      _size(0)
    {
      for(; first != last; first++)
      {
        add_sample(*first);
        _size++;
      }
    };


    void add_sample(const T& val)
    {
      _size++;
      auto mit = _map.find(val);
      if(mit != _map.end())
      {
        mit->second++;
      }
      else
      {
        _map[val] = 1;
      }
    }

    void remove_sample(const T& val)
    {
      _size--;
      auto mit = _map.find(val);
      if(mit != _map.end())
      {
        mit->second--;
      }
      else
      {
        _map[val] = 0;
      }
    }

    double get_entropy()
    {
      double total = 0.0;
      for(const auto& it : _map)
      {
        const double res = it.second/_size;
        total += res * std::log(res);
      }

      return total;
    }

};



}
}


#endif
