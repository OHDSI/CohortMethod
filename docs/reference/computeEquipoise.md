# Compute fraction in equipoise

Compute fraction in equipoise

## Usage

``` r
computeEquipoise(data, equipoiseBounds = c(0.3, 0.7))
```

## Arguments

- data:

  A data frame with at least the two columns described below.

- equipoiseBounds:

  The bounds on the preference score to determine whether a subject is
  in equipoise.

## Value

A numeric value (fraction in equipoise) between 0 and 1.

## Details

Computes the fraction of the population (the union of the target and
comparator cohorts) who are in clinical equipoise (i.e. who had a
reasonable chance of receiving either target or comparator, based on the
baseline characteristics).

The data frame should have a least the following two columns:

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group

- propensityScore (numeric): Propensity score

## References

Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger
VL, Stang P, and Schneeweiss S. (2013) A tool for assessing the
feasibility of comparative effectiveness research, Comparative Effective
Research, 3, 11-20
