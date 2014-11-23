/**
 * @file IsSorted.cpp
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

#ifndef __IsSorted_cpp__
#define __IsSorted_cpp__

#include <Rcpp.h>
#include <iostream>
#include "IsSorted.h"


using namespace Rcpp ;

namespace ohdsi {
	namespace cohortMethod {

		bool IsSorted::isSorted(const DataFrame& dataFrame,const std::vector<std::string>& indexes,const std::vector<bool>& ascending){      
			unsigned int nrows = dataFrame.nrows();
      unsigned int ncols = indexes.size();
			if (nrows == 1){
				return true;
			}
			std::vector<NumericVector> columns(ncols);
      
      for (unsigned int column=0; column<ncols; column++){
        columns[column] = dataFrame[indexes.at(column)]; // Checked: this does not make a copy of the data, it just copies the pointer       
      }

			for(unsigned int row=1; row<nrows; row++) {
        for (unsigned int column=0; column<ncols; column++){
          if (ascending.at(column)){
            if (columns[column][row] > columns[column][row-1]){
              break;
            } else if (columns[column][row] < columns[column][row-1]){
              return false;
            }
          } else {
             if (columns[column][row] < columns[column][row-1]){
              break;
            } else if (columns[column][row] > columns[column][row-1]){
              return false;
            }           
          }
				}
			}
			return true;
		}
	}
}

#endif // __IsSorted_cpp__
