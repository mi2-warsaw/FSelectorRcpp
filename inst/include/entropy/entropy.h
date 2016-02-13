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

  public:

    RollEntropy() {};
    template<class InputIterator> RollEntropy(InputIterator first, InputIterator last)
    {
      for(; first != last; first++)
      {
        add_sample(*first);
      }
    };


    void add_sample(const T& val)
    {
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
      return freq_entropy(_map.begin(), _map.end());
    }

};



}
}


#endif
