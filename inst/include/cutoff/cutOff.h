#ifndef FSELECTOR_CUTOFF_H
#define FSELECTOR_CUTOFF_H

#include <map>
#include <vector>
#include <algorithm>

namespace fselector
{

namespace cutoff
{
	// Helper for sorting paris<string, T2>
	template<typename T>
	struct sortPairsDescending 
	{
		bool operator()(const std::pair<std::string, T> &left, const std::pair<std::string, T> &right)
		{
			return left.second > right.second;
		}
	};


	template<typename T>
	struct sortPairsAscending
	{
		bool operator()(const std::pair<std::string, T> &left, const std::pair<std::string, T> &right)
		{
			return left.second < right.second;
		}
	};


	// Main template
	template<typename T1, typename T2>
	std::vector<T1> cutOff_k(std::vector<T1> &v1, std::vector<T2> &v2, double k, bool Descending = true)
	{
		// Initialize final object
		std::vector< std::pair<T1, T2> > result;
		std::vector< T1 > final_result;

		// Defining iterators for v1 (names) and v2 (values)
		typename std::vector<T1>::iterator v1It = v1.begin();
		typename std::vector<T1>::iterator v1End = v1.end();
		typename std::vector<T2>::iterator v2It = v2.begin();

		// Merging
		for (; v1It != v1End; v1It++, v2It++)
		{
			result.push_back(std::make_pair(*v1It, *v2It));
		}

		// Sorting object
		if (Descending)
			std::sort(result.begin(), result.end(), sortPairsDescending<T2>());
		else
			std::sort(result.begin(), result.end(), sortPairsAscending<T2>());


		// Triming result for final_result
		if (k > 1)
		{
			for (int i = 0; i < k; i++)
			{
				final_result.push_back(result[i].first);
			}
		}
		else if (k == 1)
		{
			for (int i = 0; i < v1.size(); i++)
			{
				final_result.push_back(result[i].first);
			}
		}
		else if (k > 0)
		{
			for (int i = 0; i < std::floor(k*v1.size()); i++)
			{
				final_result.push_back(result[i].first);
			}
		}

		return final_result;
	};


	}
}

#endif
