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
    result += (1-pow(sBaseline[0],s))*pow(cBaseline[0],t);
    for (int j=1; j<M; j++) {
      result += (pow(sBaseline[j-1],s)-pow(sBaseline[j],s))*pow(cBaseline[j],t);
    }
  }

  return result/N;
}

// [[Rcpp::export(".generateEventTimesHelper")]]
std::vector<int> generateEventTimesHelper(std::vector<double> value, std::vector<double> baseline) {
  int N = value.size();
  int M = baseline.size();
  std::vector<int> result (N,M+1);
  int j = 0;

  for (int i=0; i<M; i++) {
    while(j<N && value[j]>baseline[i]) {
      result[j] = i+1;
      j++;
    }
    continue;
  }

  return result;
}

// [[Rcpp::export(".generateEventTimesHelper1")]]
std::vector<double> generateEventTimesHelper1(std::vector<double> value, std::vector<double> baseline, std::vector<double> times) {
  int N = value.size();
  int M = baseline.size();
  std::vector<double> result (N,times[M-1]);
  int j = 0;
  double d;
  double x;

  for (int i=1; i<M; i++) {
    d = (times[i]-times[i-1])/(baseline[i]-baseline[i-1]);
    x = times[i-1]-baseline[i-1]*d;
    while(j<N && value[j]>baseline[i]) {
      result[j] = x+value[j]*d;
      j++;
    }
    continue;
  }

  return result;
}

#endif // __RcppWrapper_cpp__
