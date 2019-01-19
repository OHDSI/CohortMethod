/**
 * @file AdjustedKm.h
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

#ifndef __AdjustedKm_h__
#define __AdjustedKm_h__

#include <vector>
#include <cstdint>

namespace ohdsi {
	namespace cohortMethod {

		struct Surv {
		  Surv(std::vector<int> _time, std::vector<double> _s, std::vector<double> _var) :
		  time(_time), s(_s), var(_var) {}
      std::vector<int> time;
		  std::vector<double> s;
		  std::vector<double> var;
		};

		struct AdjustedKm {
		public:
			static Surv surv(const std::vector<double> &weight, const std::vector<int> &time, const std::vector<int> &y);
		};
	}
}

#endif // __AdjustedKm_h__
