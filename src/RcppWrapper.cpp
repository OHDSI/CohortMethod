/**
 * @file RcppWrapper.cpp
 *
 * This file is part of CohortMethod
 *
 * Copyright 2015 Observational Health Data Sciences and Informatics
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @author Observational Health Data Sciences and Informatics
 * @author Martijn Schuemie
 * @author Marc Suchard
 */

#ifndef __RcppWrapper_cpp__
#define __RcppWrapper_cpp__


#include <Rcpp.h>
#include <map>
#include "Match.h"
#include "Auc.h"
#include "BySum.h"

using namespace Rcpp;

// [[Rcpp::export(".matchOnPs")]]
DataFrame matchOnPs(std::vector<double> propensityScores, std::vector<int> treatment, unsigned int maxRatio, double caliper) {

	using namespace ohdsi::cohortMethod;

	try {
		std::vector<int64_t> stratumIds = Match::match(propensityScores, treatment, maxRatio, caliper);
		return DataFrame::create(_["propensityScore"] = propensityScores, _["treatment"] = treatment,_["stratumId"] = stratumIds);
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
	return DataFrame::create();
}

// [[Rcpp::export(".aucWithCi")]]
std::vector<double> aucWithCi(std::vector<double> propensityScores, std::vector<int> treatment) {

  using namespace ohdsi::cohortMethod;

	try {
		std::vector<double> auc = Auc::aucWithCi(propensityScores, treatment);
		return auc;
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
  std::vector<double> auc(3,0);
	return auc;
}

// [[Rcpp::export(".auc")]]
double auc(std::vector<double> propensityScores, std::vector<int> treatment) {

  using namespace ohdsi::cohortMethod;

  try {
		double auc = Auc::auc(propensityScores, treatment);
		return auc;
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
	return 0.0;
}

// [[Rcpp::export(".bySum")]]
DataFrame bySum(List ffValues, List ffBins) {

  using namespace ohdsi::cohortMethod;

  try {
    std::map<double,double> map = BySum::bySum(ffValues, ffBins);
    std::vector<double> bins;
    std::vector<double> sums;
    for(std::map<double,double>::iterator iter = map.begin(); iter != map.end(); ++iter){
      bins.push_back(iter->first);
      sums.push_back(iter->second);
    }
    return DataFrame::create(_["bins"] = bins, _["sums"] = sums);
  } catch (std::exception &e) {
    forward_exception_to_r(e);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return DataFrame::create();
}

// [[Rcpp::export(".findOutcomePrevalence")]]
double findOutcomePrevalence(std::vector<double> sBaseline, std::vector<double> sExp, std::vector<double> cBaseline, std::vector<double> cExp) {
  int M = sBaseline.size();
  int N = sExp.size();
  double result = 0;

  std::vector<double> sCopy;
  std::vector<double> cCopy;
  double s;
  double t;

  for (int i=0; i<N; i++) {
    s = sExp[i];
    t = cExp[i];
    result -= pow(sBaseline[0], s)*pow(cBaseline[0], t);
    for (int j=1; j<M; j++) {
      result += pow(sBaseline[j],s)*(pow(cBaseline[j-1], t) - pow(cBaseline[j],t));
    }
    result += pow(cBaseline[M-1], t);
  }

  return result/N;
}

#endif // __RcppWrapper_cpp__
