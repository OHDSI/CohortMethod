#include <iostream>

#include "match.h"

//#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::cohortMethod;
	std::vector<int> treat;
	std::vector<int> rowId;
	std::vector<double> ps;

	std::vector<int> stratumIds = Match::match(ps,treat,rowId,100,0.05);

	std::cout << stratumIds.size();
	return 0;
}

//#endif
