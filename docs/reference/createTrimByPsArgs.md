# Create a parameter object for the function [`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)

Create a parameter object for the function
[`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)

## Usage

``` r
createTrimByPsArgs(
  trimFraction = NULL,
  equipoiseBounds = NULL,
  maxWeight = NULL,
  trimMethod = "symmetric"
)
```

## Arguments

- trimFraction:

  For `trimFraction = symmetric`: the PS cut-off value. For
  `trimFraction = asymmetric` or `reverse asymmetric`: the fraction that
  will be removed from each treatment group. See `trimMethod` for more
  details.

- equipoiseBounds:

  A 2-dimensional numeric vector containing the upper and lower bound on
  the preference score (Walker, 2013) for keeping persons.

- maxWeight:

  The maximum allowed IPTW.

- trimMethod:

  The trimming method to be performed. Three methods are supported:

  - symmetric: trims all units with estimated PS outside the interval
    (`trimFraction`,1-`trimFraction`), following Crump et al. (2009).

  - asymmetric: removes all units not in the overlap PS range and trims
    the `trimFraction` target persons with the lowest propensity scores
    and comparator persons with the highest propensity scores, following
    Sturmer et al. (2010).

  - reverse asymmetric: removes all units not in the overlap PS range
    and trims the `trimFraction` target persons with the highest
    propensity scores and comparator persons with the lowest propensity
    scores (not suggested).

## Value

An object of type `TrimByPsArgs`.

## Details

Create an object defining the parameter values. Set any argument to
`NULL` to not use it for trimming.

## References

Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger
VL, Stang P, and Schneeweiss S. (2013) A tool for assessing the
feasibility of comparative effectiveness research, Comparative Effective
Research, 3, 11-20

Crump, Richard K., V. Joseph Hotz, Guido W. Imbens, and Oscar A. Mitnik.
2009. Dealing with limited overlap in estimation of average treatment
effects. Biometrika 96(1): 187-199.

Sturmer T, Rothman KJ, Avorn J, Glynn RJ. Treatment effects in the
presence of unmeasured confounding: dealing with observations in the
tails of the propensity score distribution–a simulation study. Am J
Epidemiol. 2010 Oct 1;172(7):843-54.
