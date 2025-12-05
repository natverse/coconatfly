# Abbreviate fly connectomics dataset names

Abbreviate fly connectomics dataset names

## Usage

``` r
abbreviate_datasets(ds)
```

## Arguments

- ds:

  One or more dataset long names

## Value

a character vector of 2 letter abbreviations

## Examples

``` r
abbreviate_datasets(c("flywire", "flywire", "hemibrain"))
#> [1] "fw" "fw" "hb"
```
