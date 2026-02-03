# Flexible function for fetching partner data across datasets

Flexible function for fetching partner data across datasets

## Usage

``` r
cf_partners(
  ids,
  threshold = 1L,
  partners = c("inputs", "outputs"),
  bind.rows = TRUE,
  MoreArgs = list(),
  keep.all = FALSE
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

- bind.rows:

  Whether to bind data.frames for each dataset together, keeping only
  the common columns (default `TRUE` for convenience but note that some
  columns will be dropped by unless `keep.all=TRUE`).

- MoreArgs:

  Additional arguments in the form of a hierarchical list (expert use;
  see details and examples).

- keep.all:

  Whether to keep all columns when processing multiple datasets rather
  than just those in common (default=`FALSE` only keeps shared columns).

## Value

A data.frame or a named list (when `bind.rows=FALSE`)

## Details

fancr and fafbseg functions have usually used a `>` relationship for the
threshold, but here (as of May 2024) it is uniformly a `>=`
relationship.

`MoreArgs` is structured as a list with a top layer naming datasets
(using the same long names as
[`cf_ids`](https://natverse.org/coconatfly/reference/cf_ids.md). The
second (lower) layer names the arguments that will be passed to
dataset-specific functions such as
[`fafbseg::flywire_partner_summary2`](https://rdrr.io/pkg/fafbseg/man/flywire_partner_summary2.html)
and
[`malevnc::manc_connection_table`](https://natverse.org/malevnc/reference/manc_connection_table.html).

## Examples
