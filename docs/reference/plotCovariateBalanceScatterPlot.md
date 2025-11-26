# Create a scatterplot of the covariate balance

Create a scatterplot of the covariate balance, showing all variables
with balance before and after matching on the x and y axis respectively.
Requires running `computeCovariateBalance` first.

## Usage

``` r
plotCovariateBalanceScatterPlot(
  balance,
  absolute = TRUE,
  threshold = 0,
  title = "Standardized difference of mean",
  fileName = NULL,
  beforeLabel = "Before matching",
  afterLabel = "After matching",
  showCovariateCountLabel = FALSE,
  showMaxLabel = FALSE,
  showUnbalanced = FALSE
)
```

## Arguments

- balance:

  A data frame created by the `computeCovariateBalance` function.

- absolute:

  Should the absolute value of the difference be used?

- threshold:

  Show a threshold value for after matching standardized difference.

- title:

  The main title for the plot.

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function `ggsave` in the ggplot2 package for
  supported file formats.

- beforeLabel:

  Label for the x-axis.

- afterLabel:

  Label for the y-axis.

- showCovariateCountLabel:

  Show a label with the number of covariates included in the plot?

- showMaxLabel:

  Show a label with the maximum absolute standardized difference after
  matching/stratification?

- showUnbalanced:

  Show covariates that are considered unbalanced with a different color?

## Value

A ggplot object. Use the
[ggplot2::ggsave](https://ggplot2.tidyverse.org/reference/ggsave.html)
function to save to file in a different format.
