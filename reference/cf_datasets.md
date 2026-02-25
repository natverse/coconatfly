# List connectomics datasets known to coconatfly

List connectomics datasets known to coconatfly

## Usage

``` r
cf_datasets(rval = c("all", "available", "builtin", "external"))
```

## Arguments

- rval:

  Character string specifying which datasets to return. One of:

  - `"all"` (default) all known datasets (builtin + externally
    registered)

  - `"builtin"` only the datasets hardcoded into the package

  - `"external"` only datasets registered via the
    [`register_dataset`](https://natverse.org/coconat/reference/register_dataset.html)
    mechanism

## Value

A character vector of dataset names.

## See also

[`abbreviate_datasets`](https://natverse.org/coconatfly/reference/abbreviate_datasets.md),
[`cf_ids`](https://natverse.org/coconatfly/reference/cf_ids.md)

## Examples

``` r
cf_datasets()
#> [1] "flywire"   "malecns"   "manc"      "fanc"      "hemibrain" "opticlobe"
#> [7] "banc"      "yakubavnc"
cf_datasets('builtin')
#> [1] "flywire"   "malecns"   "manc"      "fanc"      "hemibrain" "opticlobe"
#> [7] "banc"      "yakubavnc"
```
