# CohortMethod: Comparative Cohort Method with Large Scale Propensity and Outcome Models

Functions for performing comparative cohort studies in an observational
database in the Observational Medical Outcomes Partnership (OMOP) Common
Data Model. Can extract all necessary data from a database. This
implements large-scale propensity scores (LSPS) as described in Tian et
al. (2018) [doi:10.1093/ije/dyy120](https://doi.org/10.1093/ije/dyy120)
, using a large set of covariates, including for example all drugs,
diagnoses, procedures, as well as age, comorbidity indexes, etc. Large
scale regularized regression is used to fit the propensity and outcome
models as described in Suchard et al. (2013)
[doi:10.1145/2414416.2414791](https://doi.org/10.1145/2414416.2414791) .
Functions are included for trimming, stratifying, (variable and fixed
ratio) matching and weighting by propensity scores, as well as
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
