#include <iostream>

#include "match.h"

//#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::cohortMethod;
	std::vector<int> treat = { 0, 1, 0, 1, 0 };
	std::vector<int> rowId = { 1, 2, 3, 4, 5 };
	std::vector<double> ps = { 0.1, 0.15, 0.4, 0.5, 0.55 };
	std::vector<int> stratumIds = Match::match(ps, treat, 100, 10);
	for (unsigned int i = 0; i < stratumIds.size(); i++)
		std::cout << stratumIds.at(i) << "\n";
	return 0;
}

//#endif
