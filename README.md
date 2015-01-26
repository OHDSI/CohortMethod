CohortMethod
===========

Introduction
============
CohortMethod is an R package for performing new-user cohort studies in an observational database in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Uses a large set of covariates for both the propensity and outcome model, including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, etc.
- Large scale regularized regression to fit the propensity and outcome models.
- Includes function for trimming, stratifying and matching on propensity scores.
- Includes diagnostic functions, including propensity score distribution plots and plots showing covariate balance before and after matching and/or trimming.
- Supported outcome models are (conditional) logistic regression, (conditional) Poisson regression, and (conditional) Cox regression.

Screenshots
===========
<table border = "">
<tr valign="top">
<td width = 50%>
  <img src="https://github.com/OHDSI/CohortMethod/blob/master/man/ps.png" alt="CohortMethod propensity score plot" title="CohortMethod propensity score plot" />
</td>
<td width = 50%>
 <img src="https://github.com/OHDSI/CohortMethod/blob/master/man/balanceScatterplot.png" alt="CohortMethod covariate balance plot" title="CohortMethod covariate balance plot" />
</td>
</tr><tr>
<td>Propensity (preference score) distribution</td><td>Covariate balance plot</td>
</tr>
</table>

Technology
============
CohortMethod is an R package, with some functions implemented in C++.

System Requirements
============
Requires R (version 3.1.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CohortMethod require Java.

Dependencies
============
 * Cyclops
 * DatabaseConnector
 * SqlRender

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. In R, use the following commands to download and install CohortMethod:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/SqlRender") 
  install_github("ohdsi/DatabaseConnector") 
  install_github("ohdsi/Cyclops") 
  install_github("ohdsi/CohortMethod") 
  ```

Getting Involved
=============
* Vignette: [Using SqlRender](https://raw.githubusercontent.com/OHDSI/CohortMethod/master/vignettes/SingleStudies.pdf)
* Package manual: [CohortMethod.pdf](https://raw.githubusercontent.com/OHDSI/CohortMethod/master/man/CohortMethod.pdf) 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements
 
License
=======
CohortMethod is licensed under Apache License 2.0

Development
===========
CohortMethod is being developed in R Studio.

###Development status
Under development: do not expect this to work

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
