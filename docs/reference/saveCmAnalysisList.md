# Save a list of CmAnalysis to file

Write a list of objects of type `CmAnalysis` to file. The file is in
JSON format.

## Usage

``` r
saveCmAnalysisList(CmAnalysisList, file)
```

## Arguments

- CmAnalysisList:

  A list of objects of type `CmAnalysis` as created using the
  [`createCmAnalysis()`](https://ohdsi.github.io/CohortMethod/reference/createCmAnalysis.md)
  function.

- file:

  The name of the file where the results will be written
