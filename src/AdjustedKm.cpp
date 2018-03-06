/**
 * @file Match.cpp
 *
 * This file is part of CohortMethod
 *
 * Copyright 2018 Observational Health Data Sciences and Informatics
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

#ifndef __AdjustedKm_cpp__
#define __AdjustedKm_cpp__

#include <cmath>
#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <algorithm>

#include "AdjustedKm.h"

namespace ohdsi {
namespace cohortMethod {

Surv AdjustedKm::surv(const std::vector<int> &stratumId, const std::vector<int> &time, const std::vector<int> &y, const unsigned int nBootstrap) {
  std::set<int> stratumIdSet(stratumId.begin(), stratumId.end());
  std::vector<int> uniqueStratumIds(stratumIdSet.begin(), stratumIdSet.end());
  unsigned int nStrata = uniqueStratumIds.size();
  std::map<int, unsigned int> stratumIdToIndex;
  for (unsigned int i = 0; i < uniqueStratumIds.size();i++)
    stratumIdToIndex[uniqueStratumIds[i]] = i;
  std::vector<int> stratumIndices(stratumId.size());
  std::vector<int> size(nStrata, 0);
  for (unsigned int i = 0; i < stratumId.size();i++) {
    int index = stratumIdToIndex[stratumId[i]];
    stratumIndices[i] = index;
    size[index]++;
  }

  std::set<int> timesSet(time.begin(), time.end());
  std::vector<int> uniqueTimes(timesSet.begin(), timesSet.end());
  std::sort(uniqueTimes.begin(), uniqueTimes.end());
  unsigned int nTimes = uniqueTimes.size();

  std::vector<double> rates(nTimes);
  std::vector<double> s(nTimes);
  std::vector<double> var(nTimes);

  std::vector<std::vector<int>> bootstrapStratumIndices(nBootstrap);
  std::vector<std::vector<double>> bootstrapRates(nBootstrap);
  for (unsigned int i = 0; i < nBootstrap; i++) {
    std::vector<int> sampledStratumIndices = std::vector<int>(nStrata);
    for (unsigned int j = 0; j < nStrata; j++) {
      sampledStratumIndices[j] = std::floor(std::rand() % nStrata);
    }
    bootstrapStratumIndices[i] = sampledStratumIndices;
    bootstrapRates[i] = std::vector<double>(nTimes);
  }
  for (unsigned int i = 0; i < nTimes;i++) {
    int timeCursor = uniqueTimes[i];
    // std::cout << "Time = " << timeCursor << "\n";
    std::vector<int> events(nStrata, 0);
    std::vector<int> atRisk(nStrata, 0);
    for (unsigned int j = 0; j < stratumIndices.size();j++) {
      int index = stratumIndices[j];
      if (y[j] == 1 && time[j] == timeCursor)
        events[index]++;
      if (time[j] >= timeCursor)
        atRisk[index]++;
    }
    double totalEvents = 0;
    double totalAtRisk = 0;
    for (unsigned int j = 0; j < nStrata; j++) {
      double weight = 1.0 / (double)size[j];
      totalEvents += (double)events[j] * weight;
      totalAtRisk += (double)atRisk[j] * weight;
    }
    rates[i] = 1.0 - (totalEvents / totalAtRisk);
    double temp = 1.0;
    for (unsigned int j = 0; j <= i; j++) {
      temp *= rates[j];
    }
    s[i] = temp;

    // Perform bootstrap to get variance
    // std::cout << "Bootstrapping\n";
    std::vector<double> bootstrapS(nBootstrap);
    double sumBootstrapS = 0;
    for (unsigned int j = 0; j < nBootstrap; j++) {
      totalEvents = 0;
      totalAtRisk = 0;
      for (unsigned int k = 0; k < nStrata; k++) {
        int index = bootstrapStratumIndices[j][k];
        double weight = 1.0 / (double)size[index];
        totalEvents += (double)events[index] * weight;
        totalAtRisk += (double)atRisk[index] * weight;
        // std::cout << "Index = " << index << ", weight = " << weight << ", events = " << totalEvents << ", at risk = " << totalAtRisk << "\n";
      }
      bootstrapRates[j][i] = 1.0 - (totalEvents / totalAtRisk);
      double temp = 1;
      for (unsigned int k = 0; k <= i; k++) {
        temp *= bootstrapRates[j][k];
      }
      bootstrapS[j] = temp;
      sumBootstrapS += temp;
      // std::cout << temp << "\n";
    }
    double meanBootstrapS = sumBootstrapS / (double)nBootstrap;
    // std::cout << "Mean: " << meanBootstrapS << "\n";
    double sumV = 0;
    for (unsigned int j = 0; j < nBootstrap; j++) {
      sumV += pow(bootstrapS[j] - meanBootstrapS, 2);
    }
    var[i] = (1.0/((double)nBootstrap - 1.0)) * sumV;
  }

  return(Surv(uniqueTimes, s, var));
}
}
}

#endif // __AdjustedKm_cpp__
