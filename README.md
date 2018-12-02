CohortMethod
============

[![Build Status](https://travis-ci.org/OHDSI/CohortMethod.svg?branch=master)](https://travis-ci.org/OHDSI/CohortMethod)
[![codecov.io](https://codecov.io/github/OHDSI/CohortMethod/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/CohortMethod?branch=master)

CohortMethod is part of the [OHDSI Methods Library](https://ohdsi.github.io/MethodsLibrary).

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
<table>
<tr valign="bottom">
<td width = 50%>

<img src="https://github.com/OHDSI/CohortMethod/raw/master/extras/ps.png"/>

</td>
<td width = 50%>
  
<img src="https://github.com/OHDSI/CohortMethod/raw/master/extras/balanceScatterplot.png"/>

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

Installation
=============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>. Once Java is installed, ensure that Java is being pathed correctly. Under environment variables in the control panel, ensure that the jvm.dll file is added correctly to the path.
3. In R, use the following commands to download and install CohortMethod:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("CohortMethod")
  ```
  
4. Optionally, run this to check if CohortMethod was correctly installed:

  ```r
  connectionDetails <- createConnectionDetails(dbms="postgresql",
                                               server="my_server.org",
                                               user = "joe",
                                               password = "super_secret")

  checkCmInstallation(connectionDetails)
  ```
  
  Where dbms, server, user, and password need to be changed to the settings for your database environment. Type
  
  ```r
  ?createConnectionDetails
  ``` 
  
  for more details on how to configure your database connection.

User Documentation
==================
* Vignette: [Single studies using the CohortMethod package](https://raw.githubusercontent.com/OHDSI/CohortMethod/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the CohortMethod package](https://raw.githubusercontent.com/OHDSI/CohortMethod/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [CohortMethod.pdf](https://raw.githubusercontent.com/OHDSI/CohortMethod/master/extras/CohortMethod.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CohortMethod/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
CohortMethod is licensed under Apache License 2.0

Development
===========
CohortMethod is being developed in R Studio.

### Development status

CohortMethod is actively being used in several studies and is ready for use.


# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
