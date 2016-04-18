#include <iostream> 
#include <list>
#include <vector>

// =========================================================================
//
// Author:			Damian Skrzypiec 
// Date:			18.04.2016 22:55:46
// E-Mail:			damian.j.skrzypiec@gmail.com
// GitHub:			@DSkrzypiec
// Description:		This header contain C++ template class "Subset" which
//					calculates set of all k-subsets for given set and given k.
//					By k-subset I mean subset of given set with length k.
//
//					Example: for auto x = Subset<int>(std::set<int> {1, 2, 3, 4}, 3);
//					Method x.GetKSubsets() returns following set:
//					{{1,2,3}, {1,2,4}, {1,3,4}, {2, 3, 4}}
//
// =========================================================================



template<typename T>
class Subset
{
public:
	// Constructors:
	Subset()
	{
		setp = {};
		kp = 0;
	};

	Subset(std::vector<T> &set, int k)
	{
		setp = set;
		kp = k;
	};

	// Public methods:
	std::list< std::vector<T> > GetKSubsets();
	void PrintKSubsets();

private:
	// Private fields:
	std::vector<T> setp;
	int kp;

	// Private method:
	std::list< std::vector<T> > GetKSubsets_hlper(std::vector<T> setp, int kp);

};



// Definition of method GetKSubset()
// The following method is the main method of class Subset and its calculate set of all k-subsets of given set.
// By k-subsets I mean subset length equal to k of given set.

template<typename T>
std::list< std::vector<T> > Subset<T>::GetKSubsets()
{
	return GetKSubsets_hlper(setp, kp);
}


// Definition of private method GetKSubset_hlper()

template<typename T>
std::list< std::vector<T> > Subset<T>::GetKSubsets_hlper(std::vector<T> s, int k)
{
	if (k == 0 || s.empty() || s.size() < k) { return{ {} }; }

	if (s.size() == k) { return{ s }; }

	auto x = *s.begin();
	s.erase(s.begin());

	std::list< std::vector<T> > result;

	for (auto & t : Subset::GetKSubsets_hlper(s, k - 1))
	{
		auto r = std::move(t);
		r.push_back(x);
		result.push_back(std::move(r));
	}

	for (auto & t : Subset::GetKSubsets_hlper(s, k))
	{
		result.push_back(std::move(t));
	}

	return result;
}



// Definition of method which prints result

template<typename T>
void Subset<T>::PrintKSubsets()
{
	auto xx = Subset<T>::GetKSubsets();

	for (auto it : xx)
	{
		for (auto j : it)
		{
			std::cout << j << " ";
		}

		std::cout << " " << std::endl;
	}
}

