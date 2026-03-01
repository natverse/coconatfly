# 2. Metadata columns and harmonisation

## Introduction

When querying metadata across multiple connectome datasets with
[`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md),
*coconatfly* returns a standardised set of columns. This vignette
documents these columns and explains how values are harmonised across
datasets.

``` r
library(coconatfly)
library(dplyr)
```

## Standard columns

The [`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md)
function returns these standard columns:

| Column     | Description                                                           |
|------------|-----------------------------------------------------------------------|
| `id`       | Neuron identifier (dataset-specific)                                  |
| `key`      | Unique identifier across all datasets (format: `dataset:id`)          |
| `dataset`  | Source dataset name                                                   |
| `class`    | Top-level cell class (optionally harmonised)                          |
| `subclass` | Cell subclass                                                         |
| `type`     | Cell type name, the finest classification level                       |
| `instance` | Individual neuron properties (typically formatted as `{type}_{side}`) |
| `side`     | Normalised to L/R/M (left/right/midline) or NA                        |
| `sex`      | M or F, based on the dataset                                          |
| `tissue`   | brain, vnc, or cns depending on dataset                               |
| `group`    | Numeric grouping of related neurons within dataset                    |

### Example: MBON01 across datasets

``` r
mbon01 <- cf_meta(cf_ids("MBON01", datasets = c("flywire", "malecns")))
#> Loading required namespace: git2r
#> Using malecns dataset `male-cns:v0.9`.
#> See ?malecns section Package Options for details.
#> For high-quality h5 bridging registrations (malecns <-> JRC2018U),
#> run mcns_download_xforms2() once to download, then
#> mcns_register_xforms2() once per session to activate.
mbon01 %>%
  select(id, class, type, side, sex, tissue, dataset, key)
#>                   id        class   type side sex tissue dataset
#> 1 720575940624117245      central MBON01    R   F  brain flywire
#> 2 720575940643309197      central MBON01    L   F  brain flywire
#> 3             520151 cb_intrinsic MBON01    L   M    cns malecns
#> 4              10013 cb_intrinsic MBON01    R   M    cns malecns
#>                     key
#> 1 fw:720575940624117245
#> 2 fw:720575940643309197
#> 3             mc:520151
#> 4              mc:10013
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

The `tissue` column indicates the region of the nervous system:

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

## Superclass/class field names

Most datasets have a multi-level annotation hierarchy from very broad
terms to terminal cell types, which represent the finest level of
conservation across datasets. Different datasets use different
conventions for names and values of these fields. We have already noted
that `type` is our default name for the finest level classification. The
flywire data model calls this `cell_type` but we always remap the column
name to type.

For malecns, the top level in the `class` hierarchy is `superclass`,
while flywire uses `super_class` and manc uses `class`. For historical
reasons, the default is to rename all of these to `class`. You can opt
to have the top level column as `superclass`. The renaming can be
controlled by the `coconatfly.use_superclass` package option.

``` r
options(coconatfly.use_superclass = TRUE)
# or
cf_meta(ids, use_superclass = TRUE)
```

This currently defaults to FALSE, but we expect a future version to use
this by default, more closely emulating the male cns.

- `class` -\> `superclass` (values like ascending_neuron)
- `subclass` -\> `class` (values like ALPN)
- `subsubclass` -\> `subclass`

## Class harmonisation

Even when the columns have the same name, values may differ. For
example, ascending neurons that project from the VNC to the brain might
be labelled:

- malecns: `ascending_neuron`
- manc: `ascending neuron`
- flywire: `ascending`

in different datasets.

### Enabling harmonisation

By default, class values are **not** harmonised, preserving the original
values in each dataset. To enable harmonisation to malecns-style values,
use the `harmonise_class` parameter or set a global option:

``` r
cf_meta(ids, harmonise_class = TRUE)

options(coconatfly.harmonise_class = TRUE)
cf_meta(ids)
```

A future version of coconatfly will make harmonisation the default.

### Example: Ascending neurons

Without harmonisation, class values differ between datasets:

``` r
an_meta <- cf_meta(cf_ids("AN07B004", datasets = c("manc", "malecns")))
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
