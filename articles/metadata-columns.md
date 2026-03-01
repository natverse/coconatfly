# 2. Metadata columns and harmonisation

## Introduction

When querying metadata across multiple connectome datasets with
[`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md),
coconatfly returns a standardised set of columns. This vignette
documents these columns and explains how values are harmonised across
datasets.

``` r
library(coconatfly)
library(dplyr)
```

## Standard columns

The [`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md)
function returns these standard columns: \| Column \| Description \|
\|——–\|————-\| \| `id` \| Neuron identifier (dataset-specific) \| \|
`key` \| Unique identifier across all datasets (format: `dataset:id`) \|
\| `class` \| Top-level cell class (optionally harmonised) \| \|
`subclass` \| Cell subclass \| \| `type` \| Cell type name \| \|
`instance` \| Individual neuron properties (typically `type_side`) \| \|
`side` \| Normalised to L/R/M (left/right/midline) or NA \| \| `sex` \|
M or F, based on the dataset \| \| `tissue` \| brain, vnc, or cns
depending on dataset \| \| `group` \| Numeric grouping of related
neurons within dataset \| \| `dataset` \| Source dataset name \|

### Example: MBON01 across datasets

``` r
mbon01 <- cf_meta(cf_ids("MBON01", datasets = c("flywire", "hemibrain")))
#> Loading required namespace: git2r
mbon01 %>%
  select(id, type, side, sex, tissue, dataset, key)
#>                   id   type side sex tissue   dataset                   key
#> 1 720575940624117245 MBON01    R   F  brain   flywire fw:720575940624117245
#> 2 720575940643309197 MBON01    L   F  brain   flywire fw:720575940643309197
#> 3          612371421 MBON01    R   F  brain hemibrain          hb:612371421
#> 4          673509195 MBON01    L   F  brain hemibrain          hb:673509195
```

## Dataset properties

### Sex

The `sex` column reflects the sex of the animal from which the dataset
was derived:

| Dataset   | Sex | Description                 |
|-----------|-----|-----------------------------|
| flywire   | F   | Female adult brain          |
| hemibrain | F   | Female adult brain          |
| opticlobe | M   | Male optic lobe             |
| malecns   | M   | Male central nervous system |
| manc      | M   | Male adult nerve cord       |
| fanc      | F   | Female adult nerve cord     |
| banc      | F   | Female adult nerve cord     |
| yakubavnc | M   | Male D. yakuba VNC          |

### Tissue

The `tissue` column indicates the tissue type:

- `brain`: Brain datasets (flywire, hemibrain, opticlobe)
- `vnc`: Ventral nerve cord datasets (manc, fanc, yakubavnc)
- `cns`: Complete CNS datasets (malecns, banc)

## Side normalisation

The `side` column is normalised to single uppercase letters:

- `L`: Left
- `R`: Right
- `M`: Midline (or center)
- `NA`: Unknown or not applicable

Input values like “left”, “Left”, “L”, “l” are all normalised to “L”.

## Class harmonisation

Different datasets use different conventions for the `class` column. For
example, ascending neurons might be labelled:

- malecns: `ascending_neuron`
- manc: `ascending neuron`
- flywire: `ascending`

### Enabling harmonisation

By default, class values are **not** harmonised to preserve original
dataset values. To enable harmonisation to malecns-style values:

``` r
options(coconatfly.harmonise_class = TRUE)
```

Or use the `harmonise_class` parameter:

``` r
cf_meta(ids, harmonise_class = TRUE)
```

### Example: Ascending neurons

Without harmonisation, class values differ between datasets:

``` r
an_meta <- cf_meta(cf_ids("AN07B004", datasets = c("manc", "malecns")))
#> Using malecns dataset `male-cns:v0.9`.
#> See ?malecns section Package Options for details.
#> For high-quality h5 bridging registrations (malecns <-> JRC2018U),
#> run mcns_download_xforms2() once to download, then
#> mcns_register_xforms2() once per session to activate.
an_meta %>%

  select(type, class, side, dataset)
#>       type            class side dataset
#> 1 AN07B004 ascending_neuron    L malecns
#> 2 AN07B004 ascending_neuron    R malecns
#> 3 AN07B004 ascending neuron    R    manc
#> 4 AN07B004 ascending neuron    L    manc
```

With harmonisation enabled:

``` r
an_meta_h <- cf_meta(cf_ids("AN07B004", datasets = c("manc", "malecns")),
                     harmonise_class = TRUE)
an_meta_h %>%
  select(type, class, side, dataset)
#>       type            class side dataset
#> 1 AN07B004 ascending_neuron    L malecns
#> 2 AN07B004 ascending_neuron    R malecns
#> 3 AN07B004 ascending_neuron    R    manc
#> 4 AN07B004 ascending_neuron    L    manc
```

### Harmonisation rules

When `harmonise_class = TRUE`, class values are mapped to malecns
conventions:

| Pattern               | Harmonised value    |
|-----------------------|---------------------|
| ascending (+ neuron)  | ascending_neuron    |
| descending (+ neuron) | descending_neuron   |
| sensory + ascending   | sensory_ascending   |
| sensory + descending  | sensory_descending  |
| efferent + ascending  | efferent_ascending  |
| sensory               | {domain}\_sensory   |
| motor                 | {domain}\_motor     |
| intrinsic, central    | {domain}\_intrinsic |
| efferent              | {domain}\_efferent  |
| optic                 | ol_intrinsic        |
| visual projection     | visual_projection   |
| visual centrifugal    | visual_centrifugal  |
| endocrine             | endocrine           |

Where `{domain}` is `cb` (central brain), `vnc`, or `ol` (optic lobe)
based on the dataset.

## Superclass option

The `coconatfly.use_superclass` option renames the class hierarchy
columns:

- `class` -\> `superclass`
- `subclass` -\> `class`
- `subsubclass` -\> `subclass`

This can be useful when working with datasets that have a deeper class
hierarchy.

``` r
options(coconatfly.use_superclass = TRUE)
# or
cf_meta(ids, use_superclass = TRUE)
```
