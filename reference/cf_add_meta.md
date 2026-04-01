# Add metadata to a dataframe containing neuron keys

Enriches a dataframe with metadata for neurons identified by key
columns. This is useful for adding metadata to partner data or other
results that contain neuron keys.

## Usage

``` r
cf_add_meta(x, keycol = "key", suffix = NULL, cols = NULL, ...)
```

## Arguments

- x:

  A data.frame containing one or more columns with keys

- keycol:

  Character vector of column names containing keys. Default `"key"`.
  When multiple columns specified, metadata is joined for each with
  corresponding suffixes.

- suffix:

  Character vector of suffixes for added columns. Must match length of
  keycol. Default generates `""` for single keycol, or `".1"`, `".2"`,
  etc. for multiple. Use `""` for no suffix.

- cols:

  Character vector of column names to add from metadata. Default `NULL`
  adds all columns. Warns if any specified columns are not found in
  metadata. The `key` column is always kept for joining.

- ...:

  Additional arguments passed to
  [`cf_meta`](https://natverse.org/coconatfly/reference/cf_meta.md)

## Value

data.frame with additional metadata columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Add metadata to partner column only
partners <- cf_partners(cf_ids(hemibrain='DA2_lPN'), threshold=10, details=FALSE)
partners_meta <- cf_add_meta(partners, keycol="post_key")

# Add metadata to both pre and post
partners_both <- cf_add_meta(partners,
  keycol = c("pre_key", "post_key"),
  suffix = c(".pre", ".post"))

# Add only type and side columns
partners_minimal <- cf_add_meta(partners, keycol="post_key",
  cols = c("type", "side"))
} # }
```
