/**
 * @file Auc.h
 *
 * This file is part of CohortMethod
 *
 * Copyright 2023 Observational Health Data Sciences and Informatics
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

#ifndef __Auc_h__
#define __Auc_h__

#include <vector>
#include <cstdint>

namespace ohdsi {
	namespace cohortMethod {


		struct Auc {
		public:
			static double auc(const std::vector<double> &propensityScores, const std::vector<int> &treatment);
			static std::vector<double> aucWithCi(const std::vector<double> &propensityScores, const std::vector<int> &treatment);
		private:
			static double mannWhitneyKernel(const double &x, const double &y);
		};
	}
}

#endif // __Auc_h__
