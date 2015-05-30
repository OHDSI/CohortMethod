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


.createManualAndVignettes <- function() {
  source("R/PackageMaintenance.R")

  # .myTidyDir('.')

  .myCheckUsagePackage("CohortMethod")
  #.myCheckUsagePackage("Cyclops")

  shell("rm man/CohortMethod.pdf")
  shell("R CMD Rd2pdf ./ --output=man/CohortMethod.pdf")

  rmarkdown::render("vignettes/SingleStudies.Rmd",
                    output_file = "../inst/doc/SingleStudies.pdf",
                    rmarkdown::pdf_document(latex_engine = "pdflatex",
                                            toc = TRUE,
                                            number_sections = TRUE))
}


.getFunctionDefinitionFromMem <- function(note){
  funcPos <- regexpr("^.*: ", note)
  func <- substr(note, funcPos, funcPos + attr(funcPos, "match.length") - 3)
  func <- gsub("\\s","", func)
  funcDef <- capture.output(getFunction(func, mustFind = FALSE))
  if (funcDef[1] == "NULL")
    return(NULL)
  else
    return (funcDef)
}

.getVariableName <- function(note) {
  varPos <- regexpr("(variable|parameter) .[a-zA-Z0-9_.-]*.", notes[i])
  var <- substr(notes[i], varPos + 10, varPos + attr(varPos, "match.length") - 2)
  return(var)
}

.myCheckUsagePackage <- function(package,
                                 ignoreHiddenFunctions = TRUE,
                                 suppressBindingKeywords = c("ggplot2", "ffwhich", "subset.ffdf", "glm")) {
  require(package, character.only = TRUE)
  # tools::checkFF('CohortMethod', verbose=TRUE)
  notes <- capture.output(codetools::checkUsagePackage(package,
                                                       suppressLocal = FALSE,
                                                       suppressParamAssigns = TRUE,
                                                       suppressParamUnused = FALSE,
                                                       suppressFundefMismatch = FALSE,
                                                       suppressLocalUnused = FALSE,
                                                       suppressNoLocalFun = FALSE,
                                                       skipWith = TRUE,
                                                       suppressUndefined = FALSE,
                                                       suppressPartialMatchArgs = FALSE))
  if (length(notes) == 0) {
    writeLines("No problems found")
    return()
  }
  newNotes <- c()
  for (i in 1:length(notes)) {
    if (regexpr("no visible binding for global variable", notes[i]) != -1) {
      filePos <- regexpr(" \\(.*\\.R:", notes[i])
      if (filePos != -1) {
        # Option 1: use file name and line number to get offending text:
        file <- substr(notes[i], filePos + 2, filePos + attr(filePos, "match.length") - 2)
        linePos <- regexpr("\\.R:.*\\)", notes[i])
        line <- substr(notes[i], linePos + 3, linePos + attr(linePos, "match.length") - 2)
        line <- strsplit(line, "-")[[1]]
        if (length(line) == 1) {
          line <- as.integer(line)
        } else {
          line <- as.integer(line[1]):as.integer(line[2])
        }
        text <- readLines(file)[line]
      } else {
        # Option 2: Get function definition from memory, and select lines with variable name
        funcDef <- .getFunctionDefinitionFromMem(notes[i])
        variableName <- .getVariableName(notes[i])
        text <- funcDef[grep(paste("(^|[^$])", variableName, sep = ""), funcDef)]
      }
      hasKeyword <- FALSE
      for (keyword in suppressBindingKeywords) {
        if (length(grep(keyword, text)) != 0)
          hasKeyword <- TRUE
      }
      if (!hasKeyword)
        newNotes <- c(newNotes, notes[i])
    } else if (regexpr("assigned but may not be used", notes[i]) != -1) {
      funcDef <- .getFunctionDefinitionFromMem(notes[i])
      if (is.null(funcDef)) {
        if (ignoreHiddenFunctions)
          warning(paste("Ignoring problem in hidden function '",func,"'",sep=""))
        else
          newNotes <- c(newNotes, notes[i])
      } else {
        variableName <- .getVariableName(notes[i])
        text <- funcDef[grep(paste("(^|[^$])", variableName, sep = ""), funcDef)]
        hasKeyword <- FALSE
        for (keyword in suppressBindingKeywords) {
          if (length(grep(keyword, text)) != 0)
            hasKeyword <- TRUE
        }
        if (!hasKeyword)
          newNotes <- c(newNotes, notes[i])
      }
    } else if (regexpr("parameter .* may not be used", notes[i]) != -1) {
      funcDef <- .getFunctionDefinitionFromMem(notes[i])
      if (is.null(funcDef)) {
        if (ignoreHiddenFunctions)
          warning(paste("Ignoring problem in hidden function '",func,"'",sep=""))
        else
          newNotes <- c(newNotes, notes[i])
      } else {
        if (length(grep("UseMethod\\(",funcDef)) == 0)
          newNotes <- c(newNotes, notes[i])
      }
    } else {
      newNotes <- c(newNotes, notes[i])
    }
  }
  if (length(newNotes) == 0)
    writeLines("No problems found") else writeLines(newNotes)
}

.mySplit <- function(fullLine,
                     anchors = c("\\("),
                     separators = c(","),
                     offsets = c(-3),
                     width.cutoff) {
  bestSolution <- c(fullLine)
  minLength <- nchar(fullLine)
  minLines <- 1

  for (i in 1:length(anchors)) {
    anchor <- anchors[i]
    separator <- separators[i]
    offset <- offsets[i]
    potentialAnchors <- gregexpr(anchor, fullLine)
    if (potentialAnchors[[1]][1] != -1) {
      quotationMark <- regexpr("\"", fullLine)
      for (potentialAnchor in potentialAnchors[[1]]) {
        if (quotationMark[[1]][1] == -1 || quotationMark[[1]][1] > potentialAnchor) {
          indent <- potentialAnchor + 1
          newX <- c()
          depth <- 0
          quote <- FALSE
          start <- 1
          for (j in indent:nchar(fullLine)) {
            char <- substr(fullLine, j, j)
            if (char == "\"") {
              quote <- !quote
            } else if (!quote) {
              if (char == "(") {
                depth <- depth + 1
              } else if (char == ")") {
                if (depth == 0)
                  break
                depth <- depth - 1
              } else if (depth == 0 & char == separator) {
                part <- substr(fullLine, start, j)
                if (start != 1) {
                  part <- paste(paste(rep(" ", indent + offset), collapse = ""), part)
                }
                newX <- c(newX, part)
                start <- j + 1
              }
            }
          }
          part <- substr(fullLine, start, nchar(fullLine))
          if (start != 1) {
            part <- paste(paste(rep(" ", indent + offset), collapse = ""), part)
          }
          newX <- c(newX, part)
          maxLength <- 0
          for (x in newX) if (nchar(x) > maxLength)
            maxLength <- nchar(x)
          lines <- length(newX)
          better <- FALSE
          if (maxLength <= width.cutoff && minLength > width.cutoff) {
            better <- TRUE
          } else if (maxLength <= width.cutoff && minLength <= width.cutoff) {
            if (lines < minLines)
              better <- TRUE
          } else {
            if (maxLength < minLength)
              better <- TRUE
          }
          if (better) {
            bestSolution <- newX
            minLength <- maxLength
            minLines <- lines
          }
        }
      }
    }
  }
  return(bestSolution)
}

.reWrapLines <- function(x, width.cutoff) {
  # Unwrap lines:
  newX <- c()
  i <- 1
  while (i <= length(x)) {
    if (regexpr("[,+-] $", x[i]) != -1 && regexpr("^\\s*#", x[i]) == -1) {
      fullLine <- c(x[i])
      i <- i + 1
      while (regexpr("[,+-] $", x[i]) != -1) {
        fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
        i <- i + 1
      }
      fullLine <- c(fullLine, substr(x[i], regexpr("[^ ]", x[i]), nchar(x[i])))
      fullLine <- paste(fullLine, collapse = "")
      newX <- c(newX, fullLine)
    } else {
      newX <- c(newX, x[i])
    }
    i <- i + 1
  }
  x <- newX

  # Rewrap lines:
  newX <- c()
  i <- 1
  while (i <= length(x)) {
    if (nchar(x[i]) > width.cutoff && regexpr("^\\s*#", x[i]) == -1) {
      newSplit <- .mySplit(x[i],
                           anchors = c("<- ", "\\("),
                           separators = c("+", ","),
                           offsets = c(-1, -3),
                           width.cutoff)
      for (j in 1:length(newSplit)) {
        if (nchar(newSplit[j]) <= width.cutoff) {
          newX <- c(newX, newSplit[j])
        } else {
          newerSplit <- .mySplit(newSplit[j],
                                 anchors = c("\\("),
                                 separators = c(","),
                                 offsets = c(-3),
                                 width.cutoff)
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

.findStartOfBody <- function(text) {
  i <- 1
  while (i <= length(text)) {
    if (regexpr("^ *(#([^']|$)|$)", text[i]) == -1)
      return(i)
    i <- i + 1
  }
  return(-1)
}
.tidyExample <- function(text, width.cutoff) {
  start <- 1
  newText <- c()
  snippet <- c()
  inDontRun <- FALSE
  level <- 0
  for (i in 1:length(text)) {
    if (!inDontRun && regexpr("\\\\dontrun *\\{", text[i]) != -1) {
      if (i > start) {
        dontrun <- regexpr("\\\\dontrun *\\{", text[i])
        snippet <- c(snippet, text[start:(i - 1)], substr(text[i], 1, dontrun[[1]]))
        newText <- c(newText, .myTidy(snippet, width.cutoff))
        snippet <- c(substr(text[i], dontrun[[1]] + attr(dontrun, "match.length"), nchar(text[i])))
        if (gsub("\\s", "", snippet[1]) == "")
          snippet <- c()
      }
      newText <- c(newText, "\\dontrun {")
      start <- i + 1
      inDontRun <- TRUE
    } else if (inDontRun) {
      for (j in 1:nchar(text[i])) {
        if (substr(text[i], j, j) == "{") {
          level <- level + 1
        } else if (substr(text[i], j, j) == "}") {
          level <- level - 1
          if (level == -1) {
            snippet <- c(snippet, text[start:(i - 1)], substr(text[i], 1, j - 1))
            newText <- c(newText, .myTidy(snippet, width.cutoff), "}")
            snippet <- c(substr(text[i], j + 1, nchar(text[i])))
            if (gsub("\\s", "", snippet[1]) == "")
              snippet <- c()
            inDontRun <- FALSE
            start <- i + 1
            level <- 0
            break
          }
        }
      }
    }
  }
  if (length(text) > start) {
    snippet <- c(snippet, text[start:length(text)])
  }
  if (length(snippet) != 0) {
    newText <- c(newText, .myTidy(snippet, width.cutoff))
  }
  return(newText)
}

.tidyRoxygenBlock <- function(text, width.cutoff) {
  # Remove #' and unwrap lines:
  newText <- c()
  line <- ""
  examples <- FALSE
  for (i in 1:length(text)) {
    chunk <- sub("^\\s*#'\\s*", "", text[i])  # Remove leading spaces and #'
    chunk <- sub("\\s*$", "", chunk)  # Remove trailing spaces

    if (regexpr("^@|$)", chunk) != -1)
      examples <- FALSE

    if (chunk == "" || regexpr("^@|$)", chunk) != -1 || examples) {
      if (!(length(newText) == 0 && line == ""))
        newText <- c(newText, line)
      line <- ""
    }
    if (regexpr("^@examples", chunk) != -1) {
      examples <- TRUE
    }

    line <- paste(line, chunk, " ", sep = "")
  }
  newText <- c(newText, line)
  text <- newText

  # Put most keywords on their own line:
  newText <- c()
  for (i in 1:length(text)) {
    if ((regexpr("^@",
                 text[i]) != -1) && (regexpr("^(@param|@template|@export|@keyword|@docType|@importFrom|@import|@useDynLib|@name)",
                                             text[i]) == -1)) {
      keyword <- regexpr("^@[a-zA-Z0-9]*", text[i])
      newText <- c(newText, substr(text[i], 1, attr(keyword, "match.length")))
      newText <- c(newText, substr(text[i], attr(keyword, "match.length") + 2, nchar(text[i])))
    } else {
      newText <- c(newText, text[i])
    }
  }
  text <- newText

  # Perform wrapping
  maxParamLength <- 0
  for (i in 1:length(text)) {
    keyword <- regexpr("^@param\\s+[a-zA-Z0-9]+", text[i])
    if (attr(keyword, "match.length") > maxParamLength)
      maxParamLength <- attr(keyword, "match.length")
  }
  examples <- FALSE
  example <- c()
  newText <- c()
  for (i in 1:length(text)) {
    if (examples) {
      if (regexpr("^@", text[i]) == -1) {
        example <- c(example, text[i])
      } else {
        examples <- FALSE
        if (length(example) != 0) {
          example <- .tidyExample(example, width.cutoff = width.cutoff - 3)
          newText <- c(newText, example, " ")
        }
        example <- c()
      }
    }
    if (!examples) {
      if (regexpr("^@param", text[i]) == -1) {
        newText <- c(newText, strwrap(text[i], width = width.cutoff))
      } else {
        param <- regexpr("^@param\\s+[a-zA-Z0-9]+", text[i])
        definition <- regexpr("^@param\\s+[a-zA-Z0-9]+\\s+", text[i])
        part1 <- substr(text[i], 1, attr(param, "match.length"))
        part2 <- substr(text[i], attr(definition, "match.length") + 1, nchar(text[i]))
        part2Wrapped <- strwrap(part2, width = width.cutoff - maxParamLength - 2)
        line1 <- paste(part1, paste(rep(" ", 3 + maxParamLength - attr(param, "match.length")),
                                    collapse = ""), part2Wrapped[1], sep = "")
        newText <- c(newText, line1)
        if (length(part2Wrapped) > 1) {
          otherLines <- paste(paste(rep(" ", 2 + maxParamLength), collapse = ""),
                              part2Wrapped[2:length(part2Wrapped)])
          newText <- c(newText, otherLines)
        }
      }
      if (regexpr("^@examples", text[i]) != -1) {
        examples <- TRUE
      }
    }
  }
  text <- paste("#'", newText)
  return(text)
}

.roxygenTidy <- function(text, width.cutoff) {
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
          newText <- c(newText, text[toAdd:(start - 1)])
        newText <- c(newText, .tidyRoxygenBlock(text[start:(i - 1)], width.cutoff = width.cutoff))
        toAdd <- i
      }
      start <- -1
    }
    i <- i + 1
  }
  if (length(text) >= toAdd) {
    if (start != -1) {
      newText <- c(newText,
                   .tidyRoxygenBlock(text[start:length(text)], width.cutoff = width.cutoff))
    } else {
      newText <- c(newText, text[toAdd:length(text)])
    }
  }
  return(newText)
}

.myTidy <- function(text, width.cutoff) {
  text <- gsub("\\t", "", text)  # Remove all tabs
  text <- capture.output(formatR::tidy_source(text = text,
                                              width.cutoff = width.cutoff,
                                              arrow = TRUE,
                                              indent = 2))
  text <- .reWrapLines(text, width.cutoff = width.cutoff)
  text <- .roxygenTidy(text, width.cutoff = width.cutoff)
}


.myTidySource <- function(file, width.cutoff = 100) {
  # Note: Github code window width is 130 characters, but 100 fits better on my laptop
  text <- readLines(file)
  startOfBody <- .findStartOfBody(text)
  if (startOfBody == 1) {
    header <- c()
  } else {
    header <- text[1:(startOfBody - 1)]
  }
  body <- text[startOfBody:length(text)]
  body <- .myTidy(body, width.cutoff = width.cutoff)
  text <- c(header, body)
  writeLines(text, con = file)
}

.myTidyDir <- function(path = ".", recursive = TRUE, ...) {
  flist <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  for (f in flist) {
    message("tidying ", f)
    .myTidySource(f)
  }
}

testCode <- function() {
  path <- paste(getwd(), "/R/test", sep = "")
  .myTidyDir(path)
  # file <- 'C:/Users/mschuemi/git/CohortMethod/man-roxygen/GetCovariatesParams.R'
  file <- "C:/Users/mschuemi/git/CohortMethod/R/HelperFunctions.R"
  file <- "C:/Users/mschuemi/git/CohortMethod/R/PackageMaintenance.R"
  # Still todo: - Code formatting of examples + Prevent wrapping of literal strings
}
