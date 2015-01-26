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
  #tidy_dir("R")

  shell("rm man/CohortMethod.pdf")
  shell("R CMD Rd2pdf ./ --output=man/CohortMethod.pdf")

  require(rmarkdown)
  rmarkdown::render("vignettes/SingleStudies.Rmd", pdf_document(latex_engine = "pdflatex",toc = TRUE,number_sections = TRUE))
}