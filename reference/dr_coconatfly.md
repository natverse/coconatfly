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
#> Warning: running command ''/home/runner/.cache/R/reticulate/uv/cache/archive-v0/SmBRTp1RXnBcn4OtyaoXs/bin/python' -m pip freeze' had status 1
#> Warning: running command ''/home/runner/.cache/R/reticulate/uv/cache/archive-v0/SmBRTp1RXnBcn4OtyaoXs/bin/python' -m pip freeze' had status 1
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
#> 2       manc:v1.2.3
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
#> The following extension datasets have also been registered:
#>        name shortname sex          idfun         metafun          partnerfun
#> 1      banc        bc   F      .banc_ids      .banc_meta      .banc_partners
#> 2      fanc        fv   F      .fanc_ids      .fanc_meta      .fanc_partners
#> 3   flywire        fw   F   .flywire_ids   .flywire_meta   .flywire_partners
#> 4 hemibrain        hb   F .hemibrain_ids .hemibrain_meta .hemibrain_partners
#> 5   malecns        mc   F   .malecns_ids   .malecns_meta   .malecns_partners
#> 6      manc        mv   F      .manc_ids      .manc_meta      .manc_partners
#> 7 opticlobe        ol   F .opticlobe_ids .opticlobe_meta .opticlobe_partners
#> 8 yakubavnc        yv   F .yakubavnc_ids .yakubavnc_meta .yakubavnc_partners
#>    namespace
#> 1 coconatfly
#> 2 coconatfly
#> 3 coconatfly
#> 4 coconatfly
#> 5 coconatfly
#> 6 coconatfly
#> 7 coconatfly
#> 8 coconatfly
# }
```
