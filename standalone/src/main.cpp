#include <iostream>
#include <cstdlib>

#include "auc.h"

//#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::cohortMethod;
	unsigned int n = 100000;
	std::vector<double> ps(n);
	std::vector<int> treat(n);
	for (unsigned int i = 0; i < n; ++i) {
		ps[i] = ((double)rand() / (double)(RAND_MAX));
		treat[i] = rand() % 2;
	}
//	std::vector<double> auc = Auc::aucWithCi(ps, treat);
//	for (unsigned int i = 0; i < auc.size(); i++)
//		std::cout << auc.at(i) << "\n";
	double auc = Auc::auc(ps, treat);
	std::cout << auc << "\n";
	return 0;
}

//#endif
