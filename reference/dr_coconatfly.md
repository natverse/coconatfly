# Status report for coconatfly installation

Status report for coconatfly installation

## Usage

``` r
dr_coconatfly()
```

## Value

A dataframe containing status information for connectome datasets
(invisibly).

## Examples

``` r
# \donttest{
dr_coconatfly()
#> # Dataset details
#> Warning: running command ''/home/runner/.cache/R/reticulate/uv/cache/archive-v0/iQv5adfo2maKG5-5FtBgS/bin/python' -m pip freeze' had status 1
#> Warning: running command ''/home/runner/.cache/R/reticulate/uv/cache/archive-v0/iQv5adfo2maKG5-5FtBgS/bin/python' -m pip freeze' had status 1
#>     dataset installed
#> 1 hemibrain      TRUE
#> 2      manc      TRUE
#> 3   malecns      TRUE
#> 4 opticlobe      TRUE
#> 5   flywire      TRUE
#> 6      fanc      TRUE
#> 7      banc      TRUE
#>                                                                      server
#> 1                                              https://neuprint.janelia.org
#> 2                                              https://neuprint.janelia.org
#> 3                                              https://neuprint.janelia.org
#> 4                                              https://neuprint.janelia.org
#> 5                                                                local dump
#> 6                                                                      <NA>
#> 7 middleauth+https://cave.fanc-fly.com/segmentation/table/wclee_fly_cns_001
#>             version
#> 1  hemibrain:v1.2.1
#> 2       manc:v1.2.1
#> 3     male-cns:v0.9
#> 4 optic-lobe:v1.0.1
#> 5               783
#> 6              <NA>
#> 7              <NA>
#> For additional information (inc auth issues) for flywire or fanc datasets, try:
#>  `fafbseg::dr_fafbseg()` or  `fancr::dr_fanc()`
#> 
#> ✖ To debug connection issues to the fanc dataset, try:
#> `fancr::dr_fanc()`
# }
```
