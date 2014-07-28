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

#include <cmath>
#include <iostream>

#include "Match.h"

//#include <queue>

namespace ohdsi {
	namespace cohortMethod {

		double Match::distance(const double score1, const double score2) {
			return fabs(score1 - score2);
		}

		std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> Match::initializeHeap(const std::vector<double> &propensityScores,
				const std::vector<int> &treatment, const std::vector<int64_t> &stratumIds) {

			std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> heap;
			for (unsigned int i = 0; i < treatment.size(); i++) {
				if (treatment.at(i) == 1) {
					//Look forward for unmatched comparator:
					unsigned int cursor = i + 1;
					while (cursor < treatment.size()) {
						if (treatment.at(cursor) == 0 && stratumIds.at(cursor) == -1) {
							heap.push(MatchPair(i, cursor, distance(propensityScores.at(i), propensityScores.at(cursor))));
							break;
						}
						cursor++;
					}
					//Look back for unmatched comparator:
					cursor = i;
					while (cursor > 0) {
						cursor--;
						if (treatment.at(cursor) == 0 && stratumIds.at(cursor) == -1) {
							heap.push(MatchPair(i, cursor, distance(propensityScores.at(i), propensityScores.at(cursor))));
							break;
						}
					}
				}
			}
			return heap;
		}

		std::vector<int64_t> Match::match(const std::vector<double> &propensityScores, const std::vector<int> &treatment, const unsigned int maxRatio,
				const double caliper) {
      int treatedCount = 0;
      for (int i = 0; i < treatment.size();i++)
        treatedCount += treatment.at(i);
      unsigned int matchedCount = 0;
			std::vector<int64_t> stratumIds(treatment.size(), -1);
			std::vector<unsigned int> stratumSizes;
			for (unsigned int targetRatio = 1; targetRatio <= maxRatio; targetRatio++) {
				std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> heap = initializeHeap(propensityScores, treatment, stratumIds);
        if (heap.empty()){
          break;
        }
				unsigned int matchedTreatedCount = 0;

					bool ranOutOfPairs = false;
					MatchPair pair = heap.top();
					heap.pop();
					while (!ranOutOfPairs && pair.distance < caliper && matchedTreatedCount < treatedCount) {
						int64_t stratumIdTreated = stratumIds.at(pair.indexTreated);
						int64_t stratumIdComparator = stratumIds.at(pair.indexComparator);
						if (stratumIdTreated == -1 && stratumIdComparator == -1) { //First time treated person is matched, comparator is unmatched
							int stratumId = stratumSizes.size();
							stratumIds.at(pair.indexTreated) = stratumId;
							stratumIds.at(pair.indexComparator) = stratumId;
							stratumSizes.push_back(1);
              matchedTreatedCount++;
              matchedCount += 2;
						} else if (stratumIdTreated != -1 && stratumIdComparator == -1) { //Already have a match for treated person, comparator is unmatched
							if (stratumSizes.at(stratumIdTreated) < targetRatio) { //We need another match for this person
								stratumIds.at(pair.indexTreated) = stratumIdTreated;
								stratumIds.at(pair.indexComparator) = stratumIdTreated;
								stratumSizes.at(stratumIdTreated) = stratumSizes.at(stratumIdTreated) + 1;
                matchedTreatedCount++;
                matchedCount++;
							}
						} else if ((stratumIdTreated == -1 || stratumSizes.at(stratumIdTreated) < targetRatio) && stratumIds.at(pair.indexComparator) != -1) {
							//We need another match for this treated person, but this comparator is already matched
							//Look back:
							int candidateBack = -1;
							unsigned int cursor = pair.indexTreated;
							while (cursor > 0 && candidateBack == -1) {
								cursor--;
								if (treatment.at(cursor) == 0 && stratumIds.at(cursor) == -1) {
									candidateBack = cursor;
								}
							}
							//Look forward:
							int candidateForward = -1;
							cursor = pair.indexTreated;
							while (cursor < treatment.size() && candidateForward == -1) {
								if (treatment.at(cursor) == 0 && stratumIds.at(cursor) == -1) {
									candidateForward = cursor;
								}
								cursor++;
							}
							if (candidateBack == -1 && candidateForward != -1) {
								double distanceForward = distance(propensityScores.at(pair.indexTreated), propensityScores.at(candidateForward));
								heap.push(MatchPair(pair.indexTreated, candidateForward, distanceForward));
							} else if (candidateBack != -1 && candidateForward == -1) {
								double distanceBack = distance(propensityScores.at(pair.indexTreated), propensityScores.at(candidateBack));
								heap.push(MatchPair(pair.indexTreated, candidateBack, distanceBack));
							} else if (candidateBack != -1 && candidateForward != -1) {
								double distanceBack = distance(propensityScores.at(pair.indexTreated), propensityScores.at(candidateBack));
								double distanceForward = distance(propensityScores.at(pair.indexTreated), propensityScores.at(candidateForward));
								if (distanceBack < distanceForward) {
									heap.push(MatchPair(pair.indexTreated, candidateBack, distanceBack));
								} else {
									heap.push(MatchPair(pair.indexTreated, candidateForward, distanceForward));
								}
							}
						}

						if (heap.empty()) {
							ranOutOfPairs = true;
						} else {
							pair = heap.top();
							heap.pop();
						}
					} //end while
          if (matchedCount == treatment.size()) { //Everyone is matched: stop
            break;
          }
          if (matchedTreatedCount == 0){ //No person was matched this round
            break;
          }
			}
			return stratumIds;
		}
	}
}

#endif // __Match_cpp__
