# Convert untyped list to SccsAnalysesSpecifications

Convert untyped list to SccsAnalysesSpecifications

## Usage

``` r
convertUntypedListToCmAnalysesSpecifications(untypedList)
```

## Arguments

- untypedList:

  A list of untyped objects. For example, these could be objects from a
  call to
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
  Importantly, `simplifyDataFrame` must be set to `FALSE` when doing so.

## Value

An object of type `SccsAnalysesSpecifications`.
