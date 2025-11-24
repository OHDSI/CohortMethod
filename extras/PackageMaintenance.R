# Copyright 2025 Observational Health Data Sciences and Informatics
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

# Manually delete package from library. Avoids "Already in use" message when rebuilding
unloadNamespace("CohortMethod")
.rs.restartR()
folder <- system.file(package = "CohortMethod")
folder
unlink(folder, recursive = TRUE, force = TRUE)
file.exists(folder)

# Format and check code:
styler::style_pkg()
OhdsiRTools::checkUsagePackage("CohortMethod")
OhdsiRTools::updateCopyrightYearFolder()
OhdsiRTools::findNonAsciiStringsInFolder()
devtools::spell_check()

# Create manual and vignettes:
unlink("extras/CohortMethod.pdf")
system("R CMD Rd2pdf ./ --output=extras/CohortMethod.pdf")

rmarkdown::render("vignettes/SingleStudies.Rmd",
                  output_file = "../inst/doc/SingleStudies.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/MultipleAnalyses.Rmd",
                  output_file = "../inst/doc/MultipleAnalyses.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/ResultsSchema.Rmd",
                  output_file = "../inst/doc/ResultsSchema.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

ResultModelManager::generateSqlSchema(
  csvFilepath = "inst/csv/resultsDataModelSpecification.csv",
  sqlOutputPath = "extras/results_data_model.sql",
  overwrite = TRUE)
