# Use FlyWire v2 (Princeton) synapses as an extra dataset with coconatfly

Register the v2 connectivity information (Princeton synapse) for FlyWire
as an additional dataset for use with coconatfly for across dataset
connectome analysis.

## Usage

``` r
register_flywire2(name = "fx", ..., showerror = TRUE)
```

## Arguments

- name:

  The dataset name to use.

- ...:

  Additional arguments passed to
  `coconat::`[`register_dataset`](https://natverse.org/coconat/reference/register_dataset.html)

- showerror:

  Logical: When set to FALSE will error out silently instead of showing
  error messages.

## Details

The v2 synapses still reference the same v783 segmentation and metadata
but

## Examples

``` r
if (FALSE) { # \dontrun{
library(coconatfly)
# once per session
register_flywire2()

# examples of within dataset analysis
dna02meta <- cf_meta(cf_ids(fx='/type:DNa02', flywire='/type:DNa02'))
dna02.comp=cf_partner_summary(dna02meta, partners = 'out', threshold = 10)
cf_ids(banc='/type:DNa.+')

# an example of across dataset cosine similarity plot
cf_cosine_plot(cf_ids('/type:LAL0(08|09|10|42)',
  datasets = c("flywire", "fx")))
} # }
```
