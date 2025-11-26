# Plot covariate prevalence

Plot prevalence of binary covariates in the target and comparator
cohorts, before and after matching. Requires running
`computeCovariateBalance` first.

## Usage

``` r
plotCovariatePrevalence(
  balance,
  threshold = 0,
  title = "Covariate prevalence",
  fileName = NULL,
  beforeLabel = "Before matching",
  afterLabel = "After matching",
  targetLabel = "Target",
  comparatorLabel = "Comparator"
)
```

## Arguments

- balance:

  A data frame created by the `computeCovariateBalance` function.

- threshold:

  A threshold value for standardized difference. When exceeding the
  threshold, covariates will be marked in a different color. If
  `threshold = 0`, no color coding will be used.

- title:

  The main title for the plot.

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function `ggsave` in the ggplot2 package for
  supported file formats.

- beforeLabel:

  Label for the before matching / stratification panel.

- afterLabel:

  Label for the after matching / stratification panel.

- targetLabel:

  Label for the x-axis.

- comparatorLabel:

  Label for the y-axis.

## Value

A ggplot object. Use the
[ggplot2::ggsave](https://ggplot2.tidyverse.org/reference/ggsave.html)
function to save to file in a different format.
