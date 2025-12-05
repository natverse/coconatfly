# Summarise the connectivity of a set of neurons grouping by type

Summarise the connectivity of a set of neurons grouping by type

## Usage

``` r
cf_partner_summary(
  ids,
  threshold = 1L,
  partners = c("inputs", "outputs"),
  aggregate.query = TRUE,
  normalise = FALSE,
  group = "type",
  rval = c("data.frame", "sparse", "matrix"),
  MoreArgs = list()
)
```

## Arguments

- ids:

  A list of ids named by the relevant datasets (see examples) or any
  other input that can be processed by the
  [`keys`](https://natverse.org/coconatfly/reference/keys.md) function
  (including a `hclust` dendrogram object.)

- threshold:

  return only edges with at least this many matches. 0 is an option
  since neuprint sometimes returns 0 weight edges.

- partners:

  Whether to return inputs or outputs

- aggregate.query:

  Whether to aggregate all query neurons of the same type (the default)
  or when `aggregate.query=FALSE` only to aggregate the partner neurons.

- normalise:

  Whether to normalise the reported weights as a fraction of the total
  for each query cell type (or individual query neuron when
  `aggregate.query=TRUE`).

- group:

  Name of the column to use for grouping. Defaults to type but other
  options could be useful e.g. class or group.

- rval:

  Choose what the function will return. `sparse` and `matrix` return
  sparse and dense (standard) matrices, respectively.

- MoreArgs:

  Additional arguments in the form of a hierarchical list (expert use;
  see details and examples).

## Value

a data.frame or (sparse) matrix based on `rval`. The column `n` refers
to the number of *partner* neurons.

## Details

This function currently groups by dataset, and pre and postsynaptic
type. It does not currently group by side. The forms returning matrices
rely on
`coconat::`[`partner_summary2adjacency_matrix`](https://natverse.org/coconat/reference/partner_summary2adjacency_matrix.html).

## Examples

``` r
if (FALSE) { # \dontrun{
lal78in=cf_partner_summary(cf_ids("/type:LAL00[78]"), threshold=10, partners='in')
lal78in
lal78in %>%
  tidyr::pivot_wider(id_cols = c(type.pre,dataset),
    names_from = type.post, values_from = weight, values_fill = 0)
lal78in %>%
  tidyr::pivot_wider(id_cols = c(type.pre),
    names_from = c(type.post,dataset), values_from = weight, values_fill = 0)
} # }
```
