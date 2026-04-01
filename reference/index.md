# Package index

## Neuron IDs

Functions for specifying and manipulating neuron IDs across datasets

- [`cf_ids()`](https://natverse.org/coconatfly/reference/cf_ids.md)
  [`c(`*`<cidlist>`*`)`](https://natverse.org/coconatfly/reference/cf_ids.md)
  : Specify ids for fly connectome datasets
- [`keys()`](https://natverse.org/coconatfly/reference/keys.md)
  [`keys2df()`](https://natverse.org/coconatfly/reference/keys.md)
  [`keys2list()`](https://natverse.org/coconatfly/reference/keys.md) :
  Interconvert between keys and ids/datasets

## Metadata and connectivity

Fetch neuron metadata and synaptic partner information

- [`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md) :
  Fetch metadata for neurons from connectome datasets
- [`cf_add_meta()`](https://natverse.org/coconatfly/reference/cf_add_meta.md)
  : Add metadata to a dataframe containing neuron keys
- [`cf_partners()`](https://natverse.org/coconatfly/reference/cf_partners.md)
  : Flexible function for fetching partner data across datasets
- [`cf_partner_summary()`](https://natverse.org/coconatfly/reference/cf_partner_summary.md)
  : Summarise the connectivity of a set of neurons grouping by type
- [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  [`multi_connection_table()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  : Multi dataset cosine clustering

## Visualisation

Plot and visualise connectivity data

- [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  [`multi_connection_table()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  : Multi dataset cosine clustering
- [`triple_cosine_plot()`](https://natverse.org/coconatfly/reference/triple_cosine_plot.md)
  : Cosine cluster across hemibrain and flywire

## Datasets and setup

List, abbreviate, and register connectome datasets

- [`cf_datasets()`](https://natverse.org/coconatfly/reference/cf_datasets.md)
  : List connectomics datasets known to coconatfly
- [`abbreviate_datasets()`](https://natverse.org/coconatfly/reference/abbreviate_datasets.md)
  : Abbreviate fly connectomics dataset names
- [`dr_coconatfly()`](https://natverse.org/coconatfly/reference/dr_coconatfly.md)
  : Status report for coconatfly installation
- [`register_flywire2()`](https://natverse.org/coconatfly/reference/register_flywire2.md)
  : Use FlyWire v2 (Princeton) synapses as an extra dataset with
  coconatfly
