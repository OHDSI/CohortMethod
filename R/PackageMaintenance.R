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
  #formatR::tidy_dir("R/test", width.cutoff = 100, arrow = TRUE, indent = 2)
  #tidySomeMore("R/test/PsFunctions.R")
  #tidySomeMore("R/test/GetCovariates.R")
  #tidySomeMore("R/test/OutcomeModels.R")

  shell("rm man/CohortMethod.pdf")
  shell("R CMD Rd2pdf ./ --output=man/CohortMethod.pdf")

  rmarkdown::render("vignettes/SingleStudies.Rmd",
                    output_file = "../inst/doc/SingleStudies.pdf",
                    rmarkdown::pdf_document(latex_engine = "pdflatex",toc = TRUE,number_sections = TRUE))
}

mySplit <- function(fullLine, anchor = "\\(", separator = ",", offset = -3) {
  newX <- c()
  indent <- regexpr(anchor, fullLine)[1] + 1
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
      } else if (depth == 0 & char == separator) {
        part <- substr(fullLine, start, j)
        if (start != 1){
          part <- paste(paste(rep(" ", indent + offset), collapse = ""), part)
        }
        newX <- c(newX, part)
        start <- j + 1
      }
    }
  }
  part <- substr(fullLine, start, nchar(fullLine))
  if (start != 1){
    part <- paste(paste(rep(" ", indent+ offset), collapse = ""), part)
  }
  newX <- c(newX, part)
  return(newX)
}

reWrapLines <- function(x, width.cutoff = 100){
  newX <- c()
  i <- 1
  while (i <= length(x)) {
    if (regexpr(", $", x[i]) != -1 | regexpr("\\+ $", x[i]) != -1){
      # Found line that has been wrapped. Recreate full line:
      fullLine <- c(x[i])
      i <- i + 1
      while (regexpr(", $", x[i]) != -1 | regexpr("\\+ $", x[i]) != -1) {
        fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
        i <- i + 1
      }
      fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
      fullLine <- paste(fullLine, collapse = "")

      newSplit <- mySplit(fullLine, anchor = "<- ", separator = "+", offset = -1)
      for (j in 1:length(newSplit)){
        if (nchar(newSplit[j]) <= width.cutoff) {
          newX <- c(newX, newSplit[j])
        } else {
          newerSplit <- mySplit(newSplit[j], anchor = "\\(", separator = ",", offset = -3)
          newX <- c(newX, newerSplit)
        }
      }
    } else {
      newX <- c(newX, x[i])
    }
    i <- i + 1
  }
  return(newX)
}

findStartOfBody <- function(text){
  i <- 1
  while (i <= length(text)) {
    if (regexpr("^ *(#([^']|$)|$)", text[i]) == -1)
      return(i)
    i = i + 1
  }
  return(-1)
}

tidyRoxygenBlock <- function(text, width.cutoff){
  # Remove #' and unwrap lines:
  newText <- c()
  line <- ""
  for (i in 1:length(text)){
    chunk <- sub("^\\s*#'\\s*", "", text[i]) # Remove leading spaces and #'
    chunk <- sub("\\s*$", "", chunk) # Remove trailing spaces

    if (line == " " || regexpr("^@|$)", chunk) != -1) {
      newText <- c(newText, line)
      line <- ""
    }
    line <- paste(line, chunk, " ", sep = "")
  }
  newText <- c(newText, line)
  text <- newText

  # Put most keywords on their own line:
  newText <- c()
  for (i in 1:length(text)){
    if ((regexpr("^@", text[i]) != -1) && (regexpr("^(@param|@template|@export|@keyword)", text[i]) == -1) ) {
      keyword <- regexpr("^@[a-z0-9]*", text[i])
      newText <- c(newText, substr(text[i], 1, attr(keyword, "match.length")))
      newText <- c(newText, substr(text[i], attr(keyword, "match.length")+2, nchar(text[i])))
    } else {
      newText <- c(newText, text[i])
    }
  }
  text <- newText

  # Perform wrapping
  maxParamLength <- 0
  for (i in 1:length(text)){
    keyword <- regexpr("^@param\\s+[a-zA-Z0-9]+", text[i])
    if (attr(keyword, "match.length") > maxParamLength)
      maxParamLength <- attr(keyword, "match.length")
  }
  newText <- c()
  for (i in 1:length(text)){
    if (regexpr("^@param", text[i]) == -1){
      newText <- c(newText, strwrap(text[i], width = width.cutoff))
    } else {
      param <- regexpr("^@param\\s+[a-zA-Z0-9]+", text[i])
      definition <- regexpr("^@param\\s+[a-zA-Z0-9]+\\s+", text[i])
      part1 <- substr(text[i], 1, attr(param, "match.length"))
      part2 <- substr(text[i], attr(definition, "match.length")+1, nchar(text[i]))
      part2Wrapped <- strwrap(part2, width = width.cutoff - maxParamLength - 2)
      line1 <- paste(part1, paste(rep(" ", 3 + maxParamLength - attr(param, "match.length")), collapse=""), part2Wrapped[1], sep="")
      newText <- c(newText, line1)
      if (length(part2Wrapped) > 1){
        otherLines <- paste(paste(rep(" ", 2 + maxParamLength) , collapse=""), part2Wrapped[2:length(part2Wrapped)])
        newText <- c(newText, otherLines)
      }
    }
  }
  text <- paste("#'", newText)
  return(text)
}

roxygenTidy <- function(text, width.cutoff = 100){
  start <- -1
  toAdd <- 1
  newText <- c()
  i <- 1
  while (i <= length(text)) {
    if (regexpr("^ *#'", text[i]) != -1) {
      if (start == -1)
        start <- i
    } else {
      if (start != -1) {
        if (start > toAdd)
          newText <- c(newText, text[toAdd:(start-1)])
        newText <- c(newText, tidyRoxygenBlock(text[start:i-1], width.cutoff = width.cutoff))
        toAdd <- i
      }
      start <- -1
    }
    i = i + 1
  }
  if (length(text) > lastAdded + 1)
    newText <- c(newText, text[toAdd:length(text)])
  return(newText)
}

myTidySource <- function(file, width.cutoff = 120) {
  text <-  readLines(file)
  startOfBody <- findStartOfBody(text)
  header <- text[1:(startOfBody-1)]
  body <- text[startOfBody:length(text)]
  body <- gsub("\\t", "", body) # Remove all tabs
  body <- capture.output(formatR::tidy_source(text = body, width.cutoff = width.cutoff, arrow = TRUE, indent = 2))
  body <- reWrapLines(body, width.cutoff = width.cutoff)
  body <- roxygenTidy(body, width.cutoff = width.cutoff)
  writeLines(c(header, body), con = file)
}
path <- paste(getwd(),"/R/test",sep="")
myTidyDir <- function (path = ".", recursive = TRUE, ...) {
  flist = list.files(path, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  for (f in flist) {
    message("tidying ", f)
    myTidySource(f)
  }
}




