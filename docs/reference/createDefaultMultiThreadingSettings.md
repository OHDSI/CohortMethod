# Create default CohortMethod multi-threading settings

Create CohortMethod multi-threading settings based on the maximum number
of cores to be used.

## Usage

``` r
createDefaultMultiThreadingSettings(maxCores)
```

## Arguments

- maxCores:

  Maximum number of CPU cores to use.

## Value

An object of type `CmMultiThreadingSettings`.

## See also

[`createMultiThreadingSettings()`](https://ohdsi.github.io/CohortMethod/reference/createMultiThreadingSettings.md)

## Examples

``` r
settings <- createDefaultMultiThreadingSettings(10)
```
