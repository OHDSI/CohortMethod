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
  #tidy_dir("R/test", width.cutoff = 100, arrow = TRUE, indent = 2)
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
  i <- 1
  while (i <= length(x)) {
    if (regexpr(", $", x[i]) != -1){
      # Found line that has been wrapped. Recreate full line:
      fullLine <- c(x[i])
      i <- i + 1
      while (regexpr(", $", x[i]) != -1) {
        fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
        i <- i + 1
      }
      fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
      fullLine <- paste(fullLine, collapse = "")
      # Now redo split in a prettier way:
      indent <- regexpr("\\(", fullLine)[1] + 1
      depth <- 0
      quote <- FALSE
      start <- 1
      for (j in indent:nchar(fullLine)){
        char <- substr(fullLine, j, j)
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
            part <- substr(fullLine, start, j)
            if (start != 1){
              part <- paste(paste(rep(" ", indent-3), collapse = ""), part)
            }
            newX <- c(newX, part)
            start <- j + 1
          }
        }
      }
      part <- substr(fullLine, start, nchar(fullLine))
      if (start != 1){
        part <- paste(paste(rep(" ", indent-3), collapse = ""), part)
      }
      newX <- c(newX, part)
    } else {
      newX <- c(newX, x[i])
    }
    i <- i + 1
  }
  writeLines(newX, con = "R/test/PsFunctions2.R")
}
