# Export cohort method results to CSV files

Export cohort method results to CSV files

## Usage

``` r
exportToCsv(
  outputFolder,
  exportFolder = file.path(outputFolder, "export"),
  databaseId,
  minCellCount = 5,
  maxCores = 1
)
```

## Arguments

- outputFolder:

  The folder where runCmAnalyses() generated all results.

- exportFolder:

  The folder where the CSV files will written.

- databaseId:

  A unique ID for the database. This will be appended to most tables.

- minCellCount:

  To preserve privacy: the minimum number of subjects contributing to a
  count before it can be included in the results. If the count is below
  this threshold, it will be set to `-minCellCount`.

- maxCores:

  How many parallel cores should be used?

## Value

Does not return anything. Is called for the side-effect of populating
the `exportFolder` with CSV files.

## Details

This requires that
[`runCmAnalyses()`](https://ohdsi.github.io/CohortMethod/reference/runCmAnalyses.md)
has been executed first. It exports all the results in the
`outputFolder` to CSV files for sharing with other sites.
