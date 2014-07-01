/**
 * @file Match.cpp
 *
 * This file is part of CohortMethod
 *
 * Copyright 2014 Observational Health Data Sciences and Informatics
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

#ifndef __Match_cpp__
#define __Match_cpp__

#include <Match.h>
#include <cmath>
//#include <queue>

namespace ohdsi {
	namespace cohortMethod {

		double Match::distance(double score1, double score2) {
			return abs(score1 - score2);
		}

		std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> Match::initializeHeap(std::vector<double> propensityScores,
				std::vector<int> treatment) {
			std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> heap;
			for (unsigned int i = 0; i < treatment.size() - 1; i++) {
				if (treatment.at(i) == 1 && treatment.at(i + 1) == 0) {
					heap.push(MatchPair(i, i + 1, distance(propensityScores.at(i), propensityScores.at(i + 1))));
				}
			}
			for (unsigned int i = 1; i < treatment.size(); i++) {
				if (treatment.at(i) == 1 && treatment.at(i - 1) == 0) {
					heap.push(MatchPair(i, i - 1, distance(propensityScores.at(i), propensityScores.at(i - 1))));
				}
			}
			return heap;
		}

		std::vector<int> Match::match(std::vector<double> propensityScores, std::vector<int> treatment, std::vector<int> rowId, int maxRatio, double caliper) {
			std::vector<int> stratumIds(rowId.size(), -1);
			std::vector<unsigned int> stratumSizes;
			std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> heap = initializeHeap(propensityScores, treatment);
			for (int i = 0; i < maxRatio; i++) {
				unsigned int matchCount = 0;
				if (!heap.empty()) {
					bool ranOutOfPairs = false;
					MatchPair pair = heap.top();
					while (!ranOutOfPairs && pair.distance < caliper && matchCount < treatment.size()) { //First time treated person is matched
						if (stratumIds.at(pair.indexTreated) == -1 && stratumIds.at(pair.indexComparator) == -1) {
							stratumIds.assign(pair.indexTreated, stratumSizes.size());
							stratumIds.assign(pair.indexComparator, stratumSizes.size());
							stratumSizes.push_back(1);
						}

						if (heap.empty()) {
							ranOutOfPairs = true;
						} else {
							pair = heap.top();
						}
					}
				}
			}

			return stratumIds;
		}

	}
}

#endif // __Match_cpp__
