# Upload results to the database server.

Requires the results data model tables have been created using the
[`createResultsDataModel`](https://ohdsi.github.io/CohortMethod/reference/createResultsDataModel.md)
function.

## Usage

``` r
uploadResults(
  connectionDetails,
  schema,
  zipFileName,
  forceOverWriteOfSpecifications = FALSE,
  purgeSiteDataBeforeUploading = TRUE,
  tempFolder = tempdir(),
  tablePrefix = "",
  ...
)
```

## Arguments

- connectionDetails:

  An object of type `connectionDetails` as created using the
  [`createConnectionDetails`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function in the DatabaseConnector package.

- schema:

  The schema on the server where the tables have been created.

- zipFileName:

  The name of the zip file.

- forceOverWriteOfSpecifications:

  If TRUE, specifications of the phenotypes, cohort definitions, and
  analysis will be overwritten if they already exist on the database.
  Only use this if these specifications have changed since the last
  upload.

- purgeSiteDataBeforeUploading:

  If TRUE, before inserting data for a specific databaseId all the data
  for that site will be dropped. This assumes the input zip file
  contains the full data for that data site.

- tempFolder:

  A folder on the local file system where the zip files are extracted
  to. Will be cleaned up when the function is finished. Can be used to
  specify a temp folder on a drive that has sufficient space if the
  default system temp space is too limited.

- tablePrefix:

  (Optional) string to insert before table names for database table
  names

- ...:

  See ResultModelManager::uploadResults
