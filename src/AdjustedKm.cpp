/**
 * @file Match.cpp
 *
 * This file is part of CohortMethod
 *
 * Copyright 2019 Observational Health Data Sciences and Informatics
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

Surv AdjustedKm::surv(const std::vector<double> &weight, const std::vector<int> &time, const std::vector<int> &y) {
  std::set<int> timesSet(time.begin(), time.end());
  std::vector<int> uniqueTimes(timesSet.begin(), timesSet.end());
  std::sort(uniqueTimes.begin(), uniqueTimes.end());
  unsigned int nTimes = uniqueTimes.size();

  std::vector<double> s(nTimes);
  std::vector<double> var(nTimes);

  double si = 1;
  double varPart = 0; // sum(1-s/Ms) in Xie equation 4
  for (unsigned int i = 0; i < nTimes;i++) {
    int timeCursor = uniqueTimes[i];
    // std::cout << "Time = " << timeCursor << "\n";
    double totalEvents = 0;
    double totalAtRisk = 0;
    double sumWsquared = 0;
    for (unsigned int j = 0; j < weight.size();j++) {
      double subjectWeight = weight[j];
      if (y[j] == 1 && time[j] == timeCursor)
        totalEvents += subjectWeight;
      if (time[j] >= timeCursor) {
        totalAtRisk += subjectWeight;
        sumWsquared += pow(subjectWeight, 2);
      }
    }
    double rate = 1.0 - (totalEvents / totalAtRisk);
    si *= rate;
    s[i] = si;
    double m = pow(totalAtRisk, 2) / sumWsquared;
    varPart += (1 - rate) / m;
    var[i] = pow(si, 2) * varPart; // Xie equation 4
  }
  return(Surv(uniqueTimes, s, var));
}
}
}

#endif // __AdjustedKm_cpp__
