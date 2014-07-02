/**
 * @file Match.h
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

#ifndef __Match_h__
#define __Match_h__

#include <queue>
#include <vector>

namespace ohdsi {
	namespace cohortMethod {

		struct MatchPair {
			MatchPair(int _indexTreated, int _indexComparator, double _distance) :
					indexTreated(_indexTreated), indexComparator(_indexComparator), distance(_distance) {
			}

			unsigned int indexTreated;
			unsigned int indexComparator;
			double distance;
		};

		struct ComparePair {
			bool operator()(MatchPair const & p1, MatchPair const & p2) {
				return p1.distance > p2.distance;
			}
		};

		struct Match {
		public:
			static std::vector<int> match(const std::vector<double> &propensityScores, const std::vector<int> &treatment, const unsigned int maxRatio,
					const double caliper);
		private:
			static double distance(double score1, double score2);
			static std::priority_queue<MatchPair, std::vector<MatchPair>, ComparePair> initializeHeap(const std::vector<double> &propensityScores,
					const std::vector<int> &treatment, const std::vector<int> &stratumIds);
		};
	}
}

#endif // __Match_h__
