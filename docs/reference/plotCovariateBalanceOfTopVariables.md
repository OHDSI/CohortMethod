# Plot variables with largest imbalance

Create a plot showing those variables having the largest imbalance
before matching, and those variables having the largest imbalance after
matching. Requires running `computeCovariateBalance` first.

## Usage

``` r
plotCovariateBalanceOfTopVariables(
  balance,
  n = 20,
  maxNameWidth = 100,
  title = NULL,
  fileName = NULL,
  beforeLabel = "before matching",
  afterLabel = "after matching"
)
```

## Arguments

- balance:

  A data frame created by the `computeCovariateBalance` function.

- n:

  (Maximum) count of covariates to plot.

- maxNameWidth:

  Covariate names longer than this number of characters are truncated to
  create a nicer plot.

- title:

  Optional: the main title for the plot.

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function `ggsave` in the ggplot2 package for
  supported file formats.

- beforeLabel:

  Label for identifying data before matching / stratification /
  trimming.

- afterLabel:

  Label for identifying data after matching / stratification / trimming.

## Value

A ggplot object. Use the
[ggplot2::ggsave](https://ggplot2.tidyverse.org/reference/ggsave.html)
function to save to file in a different format.
