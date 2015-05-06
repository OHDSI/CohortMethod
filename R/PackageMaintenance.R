# @file PackageMaintenance
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


.createManualAndVignettes <- function(){
  #Experimental:
  #library(formatR)
  #tidy_dir("R/test",width.cutoff = 100 )
  #tidySomeMore()

  shell("rm man/CohortMethod.pdf")
  shell("R CMD Rd2pdf ./ --output=man/CohortMethod.pdf")

  rmarkdown::render("vignettes/SingleStudies.Rmd",
                    output_file = "../inst/doc/SingleStudies.pdf",
                    rmarkdown::pdf_document(latex_engine = "pdflatex",toc = TRUE,number_sections = TRUE))
}

tidySomeMore <- function(){
  x <-  readLines("R/test/PsFunctions.R")
  newX <- c()
  previousList <- FALSE
  for (i in 1:length(x)){
    if (regexpr(", $", x[i]) != -1){
      parts <- c()
      depth <- 0
      quote <- FALSE
      for (j in (nchar(x[i])-2):1) {
        char <- substr(x[i],j,j)
        if (char == "\"") {
          quote <- !quote
        } else if (!quote) {
          if (char == ")") {
            depth = depth + 1
          } else if (char == "(") {
            if (depth == 0)
              break
            depth = depth - 1
          } else if (depth == 0 & char == ",") {
            parts <- c(substr(x[i],j+1,nchar(x[i])),parts)
            x[i] <- substr(x[i], 1, j)
          }
        }
      }
      parts <- c(x[i], parts)
      newX <- c(newX, parts)
      previousList <- TRUE
    } else if (previousList) {
      parts <- c()
      depth <- 0
      quote <- FALSE
      for (j in 1:nchar(x[i])) {
        char <- substr(x[i],j,j)
        if (char == "\"") {
          quote <- !quote
        } else if (!quote) {
          if (char == "(") {
            depth = depth + 1
          } else if (char == ")") {
            if (depth == 0)
              break
            depth = depth - 1
          } else if (depth == 0 & char == ",") {
            parts <- c(parts, substr(x[i], 0,j))
            x[i] <- paste(paste(rep(" ",j),collapse=""),substr(x[i], j + 1, nchar(x[i])),sep="")
          }
        }
      }
      parts <- c(parts,x[i])
      newX <- c(newX, parts)
      previousList <- FALSE
    } else {
      newX <- c(newX, x[i])
    }
  }
  writeLines(newX, con = "R/test/PsFunctions2.R")
}
