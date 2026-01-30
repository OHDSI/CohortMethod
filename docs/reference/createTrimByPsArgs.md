# Create a parameter object for the function [`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)

Create a parameter object for the function
[`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)

## Usage

``` r
createTrimByPsArgs(
  trimFraction = NULL,
  equipoiseBounds = NULL,
  maxWeight = NULL
)
```

## Arguments

- trimFraction:

  This fraction will be removed from each treatment group. In the target
  group, persons with the highest propensity scores will be removed, in
  the comparator group person with the lowest scores will be removed.

- equipoiseBounds:

  A 2-dimensional numeric vector containing the upper and lower bound on
  the preference score (Walker, 201) for keeping persons.

- maxWeight:

  The maximum allowed IPTW.

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
