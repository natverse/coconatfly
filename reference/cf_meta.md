# Fetch metadata for neurons from connectome datasets

Fetch metadata for neurons from connectome datasets

## Usage

``` r
cf_meta(
  ids,
  bind.rows = TRUE,
  integer64 = FALSE,
  keep.all = FALSE,
  MoreArgs = list(flywire = list(type = c("cell_type", "hemibrain_type")))
)
```

## Arguments

- ids:

  A list of ids named by the relevant datasets (see examples) or any
  other input that can be processed by the
  [`keys`](https://natverse.org/coconatfly/reference/keys.md) function
  (including a `hclust` dendrogram object.)

- bind.rows:

  Whether to bind data.frames for each dataset together, keeping only
  the common columns (default `TRUE` for convenience but note that some
  columns will be dropped by unless `keep.all=TRUE`).

- integer64:

  Whether ids should be character vectors (default) or 64 bit ints (more
  compact but a little fragile as they rely on the `bit64` extension
  package.)

- keep.all:

  Whether to keep all columns when processing multiple datasets rather
  than just those in common (default=`FALSE` only keeps shared columns).

- MoreArgs:

  A named list of arguments to be passed when fetching metadata for a
  given function. See details.

## Details

`MoreArgs` should be list named by the standard dataset names (e.g. as
returned by `cf_datasets`.

## See also

[`neuprint_ids`](https://natverse.org/neuprintr/reference/neuprint_search.html)

## Examples

``` r
# \donttest{
da2meta=cf_meta(cf_ids(hemibrain='DA2_lPN'))
da2meta
#>           id pre post upstream downstream status    statusLabel    voxels
#> 1 1796817841 396  509      509       3275 Traced Roughly traced 726508881
#> 2 1796818119 511  818      818       4111 Traced Roughly traced 917547898
#> 3 1797505019 345  476      476       2932 Traced Roughly traced 450568345
#> 4 1827516355 391  548      548       3263 Traced Roughly traced 741928779
#> 5  818983130 409  562      562       3400 Traced Roughly traced 612641685
#>   cropped  instance    type lineage notes  soma side class subclass subsubclass
#> 1   FALSE DA2_lPN_R DA2_lPN   AVM02  <NA>  TRUE    R  <NA>     <NA>        <NA>
#> 2   FALSE DA2_lPN_R DA2_lPN   AVM02  <NA>  TRUE    R  <NA>     <NA>        <NA>
#> 3   FALSE DA2_lPN_R DA2_lPN   AVM02  <NA> FALSE    R  <NA>     <NA>        <NA>
#> 4   FALSE DA2_lPN_R DA2_lPN   AVM02  <NA>  TRUE    R  <NA>     <NA>        <NA>
#> 5   FALSE DA2_lPN_R DA2_lPN   AVM02  <NA> FALSE    R  <NA>     <NA>        <NA>
#>   group   dataset           key
#> 1  <NA> hemibrain hb:1796817841
#> 2  <NA> hemibrain hb:1796818119
#> 3  <NA> hemibrain hb:1797505019
#> 4  <NA> hemibrain hb:1827516355
#> 5  <NA> hemibrain  hb:818983130
# / introduces a regular expression
mbonmeta=cf_meta(cf_ids(hemibrain='/MBON.+'))
# }
```
