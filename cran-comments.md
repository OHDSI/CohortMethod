Thank you for the review. In response:

1. Added the long form of OMOP to DESCRIPTION.

2. Added two references to our papers to the DESCRIPTION.

3. Repaced all writeLines() with message().

4. Note that the vignettes do not write to the user's filespace. Instead, they check if a specific folder exists, and 
read the data there for inclusion in the vignette if it does. These data are too large to be included in the package. If
not present, the vignettes will still render, just without the plots and tables.

5. We now reset user options in the vignette.

6. We have removed the call to installed.packages().

7. To our knowledge we do not install packages in our functions, examples, or vignettes. (The call to install.packages() in MultipleAnalyses.Rmd is explicitly not evaluated).

---

## Test environments
* Ubuntu 22.04, R 4.5.2
* MacOS, R 4.5.2
* MacOS M3, 4.4.1
* Windows 10, R 4.5.2

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies

There are no downstream dependencies
