# CohortMethod: New-User Cohort Method with Large Scale Propensity and Outcome Models

Functions for performing new-user cohort studies in an observational
database in the OMOP Common Data Model. Can extract the necessary data
from a database and use a large set of covariates for both the
propensity and outcome model, including for example all drugs,
diagnoses, procedures, as well as age, comorbidity indexes, etc. Large
scale regularized regression is used to fit the propensity and outcome
models. Functions are included for trimming, stratifying, (variable and
fixed ratio) matching and weighting by propensity scores, as well as
diagnostic functions, such as propensity score distribution plots and
plots showing covariate balance before and after matching and/or
trimming. Supported outcome models are (conditional) logistic
regression, (conditional) Poisson regression, and (stratified) Cox
regression. Also included are Kaplan-Meier plots that can adjust for the
stratification or matching.

## See also

Useful links:

- <https://ohdsi.github.io/CohortMethod/>

- <https://github.com/OHDSI/CohortMethod>

- Report bugs at <https://github.com/OHDSI/CohortMethod/issues>

## Author

**Maintainer**: Martijn Schuemie <schuemie@ohdsi.org>

Authors:

- Marc Suchard

- Patrick Ryan
