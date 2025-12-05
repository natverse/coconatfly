# Interconvert between keys and ids/datasets

Neurons within a dataset will be identified by numeric ids but these may
not be unique across datasets. Therefore to make identifiers that are
unique across dataset we use `keys` of the form `"<dataset>:<id>"`.

`keys` either confirms/tidies up an existing set of keys or converts a
`list` or `data.frame` to keys.

`keys2df` produces a `data.frame` with columns `id` and `dataset`
describing the ids for each dataset. The ordering of the data.frame will
match the order of keys in the input vector.

`keys2list` converts a character vector of keys to a list of ids with
one list element for each dataset

## Usage

``` r
keys(x, idcol = "id")

keys2df(keys, integer64 = FALSE)

keys2list(keys, integer64 = FALSE)
```

## Arguments

- x:

  A list, data frame, dendrogram, or character vector specifying both
  within dataset ids and dataset names. See details and examples
  especially for character vector input.

- idcol:

  optional string naming the column containing ids

- keys:

  A character vector of keys

- integer64:

  Whether the output ids should be character vectors (the default) or
  `integer64`

## Value

For `keys` a character vector of keys of the form `"<dataset>:<id>"`.

## Details

When `x` is a character vector, this must be in one of two forms.
*Either* a vector where each element is a single key of the form
`"<dataset>:<id>"` *or* a single string containing \>=1 such keys
separated by white space or commas (e.g.
`" fw:4611686018427387904, hb:12345 "`). See examples.

As a convenience `x` may also be a `dendrogram` or `hclust` object
resulting from a clustering operation.

## See also

Other ids:
[`cf_ids()`](https://natverse.org/coconatfly/reference/cf_ids.md)

## Examples

``` r
# tidying up keys copied from somewhere else ...
keys(" fw:4611686018427387904, hb:12345 ")
#> [1] "fw:4611686018427387904" "hb:12345"              

# \donttest{
keys(cf_ids(hemibrain=12345, flywire='4611686018427387904'))
#> [1] "fw:4611686018427387904" "hb:12345"              

# NB this runs the query for hemibrain type MBON01 and then maps ids -> keys
keys(cf_ids(hemibrain='MBON01'))
#> [1] "hb:612371421" "hb:673509195"
# }
# \donttest{
keys2df(cf_ids('MBON01', datasets = c("hemibrain", "flywire")))
#>                   id   dataset
#> 1 720575940624117245   flywire
#> 2 720575940643309197   flywire
#> 3          612371421 hemibrain
#> 4          673509195 hemibrain
# }
# \donttest{
keys2list(cf_ids('MBON01', datasets = c("hemibrain", "flywire")))
#> $flywire
#> [1] "720575940624117245" "720575940643309197"
#> 
#> $hemibrain
#> [1] "612371421" "673509195"
#> 
# }
```
