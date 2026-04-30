context("Artifact Store and Content Hashing")

test_that("LocalArtifactStore can be initialized", {
  outputFolder <- tempfile(pattern = "artifactStore")
  on.exit(unlink(outputFolder, recursive = TRUE))

  store <- LocalArtifactStore$new(outputFolder)
  expect_s3_class(store, "R6")
  expect_true(inherits(store, "ArtifactStore"))
  expect_true(dir.exists(outputFolder))
})

test_that("LocalArtifactStore exists/readRDS/saveRDS work", {
  outputFolder <- tempfile(pattern = "artifactStore")
  on.exit(unlink(outputFolder, recursive = TRUE))

  store <- LocalArtifactStore$new(outputFolder)

  expect_false(store$exists("test.rds"))

  store$saveRDS(list(a = 1, b = "hello"), "test.rds")
  expect_true(store$exists("test.rds"))

  obj <- store$readRDS("test.rds")
  expect_equal(obj$a, 1)
  expect_equal(obj$b, "hello")
})

test_that("LocalArtifactStore listArtifacts works", {
  outputFolder <- tempfile(pattern = "artifactStore")
  on.exit(unlink(outputFolder, recursive = TRUE))

  store <- LocalArtifactStore$new(outputFolder)

  store$saveRDS(1, "CmData_abc123.rds")
  store$saveRDS(2, "StudyPop_def456.rds")
  store$saveRDS(3, "Ps_ghi789.rds")

  all <- store$listArtifacts()
  expect_equal(length(all), 3)

  cm <- store$listArtifacts(prefix = "CmData")
  expect_equal(length(cm), 1)
  expect_true(grepl("CmData", cm))
})

test_that("LocalArtifactStore delete works", {
  outputFolder <- tempfile(pattern = "artifactStore")
  on.exit(unlink(outputFolder, recursive = TRUE))

  store <- LocalArtifactStore$new(outputFolder)

  store$saveRDS(1, "test.rds")
  expect_true(store$exists("test.rds"))

  store$delete("test.rds")
  expect_false(store$exists("test.rds"))
})

test_that("LocalArtifactStore ensureDir creates nested directories", {
  outputFolder <- tempfile(pattern = "artifactStore")
  on.exit(unlink(outputFolder, recursive = TRUE))

  store <- LocalArtifactStore$new(outputFolder)

  store$ensureDir("Analysis_1/om_abc123.rds")
  expect_true(dir.exists(file.path(outputFolder, "Analysis_1")))
})

test_that("LocalArtifactStore getFullPath works", {
  outputFolder <- tempfile(pattern = "artifactStore")
  on.exit(unlink(outputFolder, recursive = TRUE))

  store <- LocalArtifactStore$new(outputFolder)

  path <- store$getFullPath("CmData_abc.zip")
  expect_equal(path, file.path(outputFolder, "CmData_abc.zip"))
})

test_that(".contentHash produces consistent results", {
  hash1 <- CohortMethod:::.contentHash("db1", "loadArgs", 1, 2)
  hash2 <- CohortMethod:::.contentHash("db1", "loadArgs", 1, 2)
  expect_equal(hash1, hash2)
  expect_equal(nchar(hash1), 12)
})

test_that(".contentHash differs with different inputs", {
  hash1 <- CohortMethod:::.contentHash("db1", "loadArgs", 1, 2)
  hash2 <- CohortMethod:::.contentHash("db2", "loadArgs", 1, 2)
  expect_true(hash1 != hash2)
})

test_that(".contentHash handles NULL inputs", {
  hash1 <- CohortMethod:::.contentHash("db1", NULL, 1)
  hash2 <- CohortMethod:::.contentHash("db1", NULL, 1)
  expect_equal(hash1, hash2)
})

test_that(".contentHash includes databaseId in differentiation", {
  hash1 <- CohortMethod:::.contentHash("database_A", "settings1")
  hash2 <- CohortMethod:::.contentHash("database_B", "settings1")
  expect_true(hash1 != hash2)
})

test_that("Content-addressed filenames change when settings change", {
  args1 <- createGetDbCohortMethodDataArgs(
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE
    )
  )
  args2 <- createGetDbCohortMethodDataArgs(
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE
    ),
    maxCohortSize = 50000
  )

  hash1 <- CohortMethod:::.contentHash("testDb", args1$toJson(), 1, 2, NA)
  hash2 <- CohortMethod:::.contentHash("testDb", args2$toJson(), 1, 2, NA)

  expect_true(hash1 != hash2)
})

test_that("Content-addressed filenames stable when settings identical", {
  args1 <- createGetDbCohortMethodDataArgs(
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE
    )
  )
  args2 <- createGetDbCohortMethodDataArgs(
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE
    )
  )

  hash1 <- CohortMethod:::.contentHash("testDb", args1$toJson(), 1, 2, NA)
  hash2 <- CohortMethod:::.contentHash("testDb", args2$toJson(), 1, 2, NA)

  expect_equal(hash1, hash2)
})

test_that("Cascading hashes propagate upstream changes", {
  loadHash1 <- CohortMethod:::.contentHash("db1", "loadArgs_v1", 1, 2, NA)
  loadHash2 <- CohortMethod:::.contentHash("db1", "loadArgs_v2", 1, 2, NA)

  # Study pop hash includes loadHash, so changing load args changes studyPop filename
  studyPopHash1 <- CohortMethod:::.contentHash("db1", loadHash1, "studyPopArgs", 3)
  studyPopHash2 <- CohortMethod:::.contentHash("db1", loadHash2, "studyPopArgs", 3)

  expect_true(studyPopHash1 != studyPopHash2)

  # But same load args + same studyPop args = same hash
  studyPopHash3 <- CohortMethod:::.contentHash("db1", loadHash1, "studyPopArgs", 3)
  expect_equal(studyPopHash1, studyPopHash3)
})
