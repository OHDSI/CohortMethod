# Package index

## Getting data and creating a study population

Functions for getting the necessary data from the database in Common
Data Model, and creating a study population.

- [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md)
  : Get the cohort data from the server
- [`show(`*`<CohortMethodData>`*`)`](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  [`summary(`*`<CohortMethodData>`*`)`](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  : Cohort Method Data
- [`isCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/isCohortMethodData.md)
  : Check whether an object is a CohortMethodData object
- [`saveCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/saveCohortMethodData.md)
  : Save the cohort method data to file
- [`loadCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/loadCohortMethodData.md)
  : Load the cohort method data from a file
- [`createStudyPopulation()`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)
  : Create a study population

## Propensity scores

Functions for creating and using propensity scores.

- [`createPs()`](https://ohdsi.github.io/CohortMethod/reference/createPs.md)
  : Create propensity scores
- [`getPsModel()`](https://ohdsi.github.io/CohortMethod/reference/getPsModel.md)
  : Get the propensity model
- [`matchOnPs()`](https://ohdsi.github.io/CohortMethod/reference/matchOnPs.md)
  : Match persons by propensity score
- [`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)
  : Trim persons by propensity score
- [`stratifyByPs()`](https://ohdsi.github.io/CohortMethod/reference/stratifyByPs.md)
  : Stratify persons by propensity score
- [`truncateIptw()`](https://ohdsi.github.io/CohortMethod/reference/truncateIptw.md)
  : Truncate IPTW values

## Outcome models

Functions for creating and viewing outcome models.

- [`fitOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/fitOutcomeModel.md)
  : Create an outcome model, and compute the relative risk
- [`getOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/getOutcomeModel.md)
  : Get the outcome model
- [`adjustedKm()`](https://ohdsi.github.io/CohortMethod/reference/adjustedKm.md)
  : Compute a weight-adjusted Kaplan-Meier curve
- [`plotKaplanMeier()`](https://ohdsi.github.io/CohortMethod/reference/plotKaplanMeier.md)
  : Plot the Kaplan-Meier curve

## Diagnostics

Functions for producing various study diagnostics.

- [`computeMdrr()`](https://ohdsi.github.io/CohortMethod/reference/computeMdrr.md)
  : Compute the minimum detectable relative risk
- [`getAttritionTable()`](https://ohdsi.github.io/CohortMethod/reference/getAttritionTable.md)
  : Get the attrition table for a population
- [`drawAttritionDiagram()`](https://ohdsi.github.io/CohortMethod/reference/drawAttritionDiagram.md)
  : Draw the attrition diagram
- [`plotFollowUpDistribution()`](https://ohdsi.github.io/CohortMethod/reference/plotFollowUpDistribution.md)
  : Plot the distribution of follow-up time
- [`createCmTable1()`](https://ohdsi.github.io/CohortMethod/reference/createCmTable1.md)
  : Create a table 1
- [`getDefaultCmTable1Specifications()`](https://ohdsi.github.io/CohortMethod/reference/getDefaultCmTable1Specifications.md)
  : Get the default table 1 specifications
- [`getFollowUpDistribution()`](https://ohdsi.github.io/CohortMethod/reference/getFollowUpDistribution.md)
  : Get the distribution of follow-up time
- [`plotPs()`](https://ohdsi.github.io/CohortMethod/reference/plotPs.md)
  : Plot the propensity score distribution
- [`computeEquipoise()`](https://ohdsi.github.io/CohortMethod/reference/computeEquipoise.md)
  : Compute fraction in equipoise
- [`computePsAuc()`](https://ohdsi.github.io/CohortMethod/reference/computePsAuc.md)
  : Compute the area under the ROC curve
- [`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)
  : Compute covariate balance before and after PS adjustment
- [`plotCovariateBalanceOfTopVariables()`](https://ohdsi.github.io/CohortMethod/reference/plotCovariateBalanceOfTopVariables.md)
  : Plot variables with largest imbalance
- [`plotCovariateBalanceScatterPlot()`](https://ohdsi.github.io/CohortMethod/reference/plotCovariateBalanceScatterPlot.md)
  : Create a scatterplot of the covariate balance
- [`plotCovariatePrevalence()`](https://ohdsi.github.io/CohortMethod/reference/plotCovariatePrevalence.md)
  : Plot covariate prevalence
- [`plotTimeToEvent()`](https://ohdsi.github.io/CohortMethod/reference/plotTimeToEvent.md)
  : Plot time-to-event
- [`getGeneralizabilityTable()`](https://ohdsi.github.io/CohortMethod/reference/getGeneralizabilityTable.md)
  : Get information on generalizability

## Running multiple analyses

Functions for running multiple analyses in an efficient way.

- [`createComputeCovariateBalanceArgs()`](https://ohdsi.github.io/CohortMethod/reference/createComputeCovariateBalanceArgs.md)
  :

  Create a parameter object for the function
  [`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)

- [`createCreatePsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createCreatePsArgs.md)
  :

  Create a parameter object for the function
  [`createPs()`](https://ohdsi.github.io/CohortMethod/reference/createPs.md)

- [`createCreateStudyPopulationArgs()`](https://ohdsi.github.io/CohortMethod/reference/createCreateStudyPopulationArgs.md)
  :

  Create a parameter object for the function
  [`createStudyPopulation()`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)

- [`createFitOutcomeModelArgs()`](https://ohdsi.github.io/CohortMethod/reference/createFitOutcomeModelArgs.md)
  :

  Create a parameter object for the function
  [`fitOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/fitOutcomeModel.md)

- [`createGetDbCohortMethodDataArgs()`](https://ohdsi.github.io/CohortMethod/reference/createGetDbCohortMethodDataArgs.md)
  :

  Create a parameter object for the function
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md)

- [`createMatchOnPsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createMatchOnPsArgs.md)
  :

  Create a parameter object for the function
  [`matchOnPs()`](https://ohdsi.github.io/CohortMethod/reference/matchOnPs.md)

- [`createStratifyByPsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createStratifyByPsArgs.md)
  :

  Create a parameter object for the function
  [`stratifyByPs()`](https://ohdsi.github.io/CohortMethod/reference/stratifyByPs.md)

- [`createTrimByPsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createTrimByPsArgs.md)
  :

  Create a parameter object for the function
  [`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)

- [`createTruncateIptwArgs()`](https://ohdsi.github.io/CohortMethod/reference/createTruncateIptwArgs.md)
  :

  Create a parameter object for the function
  [`truncateIptw()`](https://ohdsi.github.io/CohortMethod/reference/truncateIptw.md)

- [`createCmAnalysis()`](https://ohdsi.github.io/CohortMethod/reference/createCmAnalysis.md)
  : Create a CohortMethod analysis specification

- [`saveCmAnalysisList()`](https://ohdsi.github.io/CohortMethod/reference/saveCmAnalysisList.md)
  : Save a list of CmAnalysis to file

- [`loadCmAnalysisList()`](https://ohdsi.github.io/CohortMethod/reference/loadCmAnalysisList.md)
  : Load a list of CmAnalysis from file

- [`createOutcome()`](https://ohdsi.github.io/CohortMethod/reference/createOutcome.md)
  : Create outcome definition

- [`createTargetComparatorOutcomes()`](https://ohdsi.github.io/CohortMethod/reference/createTargetComparatorOutcomes.md)
  : Create target-comparator-outcomes combinations.

- [`saveTargetComparatorOutcomesList()`](https://ohdsi.github.io/CohortMethod/reference/saveTargetComparatorOutcomesList.md)
  :

  Save a list of `TargetComparatorOutcomes` to file

- [`loadTargetComparatorOutcomesList()`](https://ohdsi.github.io/CohortMethod/reference/loadTargetComparatorOutcomesList.md)
  :

  Load a list of `TargetComparatorOutcomes` from file

- [`createCmDiagnosticThresholds()`](https://ohdsi.github.io/CohortMethod/reference/createCmDiagnosticThresholds.md)
  : Create CohortMethod diagnostics thresholds

- [`createCmAnalysesSpecifications()`](https://ohdsi.github.io/CohortMethod/reference/createCmAnalysesSpecifications.md)
  : Create full CM analysis specifications

- [`convertUntypedListToCmAnalysesSpecifications()`](https://ohdsi.github.io/CohortMethod/reference/convertUntypedListToCmAnalysesSpecifications.md)
  : Convert untyped list to SccsAnalysesSpecifications

- [`createMultiThreadingSettings()`](https://ohdsi.github.io/CohortMethod/reference/createMultiThreadingSettings.md)
  : Create CohortMethod multi-threading settings

- [`createDefaultMultiThreadingSettings()`](https://ohdsi.github.io/CohortMethod/reference/createDefaultMultiThreadingSettings.md)
  : Create default CohortMethod multi-threading settings

- [`runCmAnalyses()`](https://ohdsi.github.io/CohortMethod/reference/runCmAnalyses.md)
  : Run a list of analyses

- [`getFileReference()`](https://ohdsi.github.io/CohortMethod/reference/getFileReference.md)
  : Get file reference

- [`getResultsSummary()`](https://ohdsi.github.io/CohortMethod/reference/getResultsSummary.md)
  : Get a summary report of the analyses results

- [`getInteractionResultsSummary()`](https://ohdsi.github.io/CohortMethod/reference/getInteractionResultsSummary.md)
  : Get a summary report of the analyses results

- [`getDiagnosticsSummary()`](https://ohdsi.github.io/CohortMethod/reference/getDiagnosticsSummary.md)
  : Get a summary report of the analyses diagnostics

- [`exportToCsv()`](https://ohdsi.github.io/CohortMethod/reference/exportToCsv.md)
  : Export cohort method results to CSV files

## Results upload

Uploading results to a database.

- [`getResultsDataModelSpecifications()`](https://ohdsi.github.io/CohortMethod/reference/getResultsDataModelSpecifications.md)
  : Get specifications for CohortMethod results data model
- [`createResultsDataModel()`](https://ohdsi.github.io/CohortMethod/reference/createResultsDataModel.md)
  : Create the results data model tables on a database server.
- [`migrateDataModel()`](https://ohdsi.github.io/CohortMethod/reference/migrateDataModel.md)
  : Migrate Data model
- [`getDataMigrator()`](https://ohdsi.github.io/CohortMethod/reference/getDataMigrator.md)
  : Get database migrations instance
- [`uploadResults()`](https://ohdsi.github.io/CohortMethod/reference/uploadResults.md)
  : Upload results to the database server.

## Simulation

Functions for simulating cohort method data objects.

- [`createCohortMethodDataSimulationProfile()`](https://ohdsi.github.io/CohortMethod/reference/createCohortMethodDataSimulationProfile.md)
  : Create simulation profile
- [`simulateCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/simulateCohortMethodData.md)
  : Generate simulated data
- [`cohortMethodDataSimulationProfile`](https://ohdsi.github.io/CohortMethod/reference/cohortMethodDataSimulationProfile.md)
  : A simulation profile

## Helper functions

Various helper functions

- [`checkCmInstallation()`](https://ohdsi.github.io/CohortMethod/reference/checkCmInstallation.md)
  : Check is CohortMethod and its dependencies are correctly installed
