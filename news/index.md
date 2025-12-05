# Changelog

## coconatfly (development version)

## coconatfly 0.2.4

### What’s Changed

- Handle missing partners in
  [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/36>
- Fill in instance column across datasets by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/39>
- warn but still complete without errors when there are no partners at
  all for inputs or ouptuts in
  [`cf_partners()`](https://natverse.org/coconatfly/reference/cf_partners.md).
  An example of when this happens is when running
  [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  for sensory neurons where all the inputs are below threshold.
  [\#36](https://github.com/natverse/coconatfly/issues/36)
- suggest malecns \>= 0.3.3
  (<https://github.com/flyconnectome/malecns/releases/tag/v0.3.3>) to
  prefer manc types for ANs + SAs (see
  <https://github.com/flyconnectome/malecns/pull/28>)

**Full Changelog**:
<https://github.com/natverse/coconatfly/compare/v0.2.3>…v0.2.4

## coconatfly 0.2.3

- empty query fixes for all keys and banc metadata by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/34>
- Fix malecns class in
  [`cf_partners()`](https://natverse.org/coconatfly/reference/cf_partners.md)
  by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/35>
- also support for rootSide in malecns in
  [\#35](https://github.com/natverse/coconatfly/issues/35)

**Full Changelog**:
<https://github.com/natverse/coconatfly/compare/v0.2.2>…v0.2.3

## coconatfly 0.2.2

- support malecns superclass/class/subclass change. To stay backward
  compatible for the time being the class hierarchy is mapped onto
  - class
  - subclass
  - subsubclass but the intent is to switch to superclass/class/subclass
    in the next version by [@jefferis](https://github.com/jefferis) in
    <https://github.com/natverse/coconatfly/pull/32>
- Correct handling of malecns lineage information by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/33>
- [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  now fetches all metadata cols by default
  (f1667a3c8318120626ae95c2116f73576fd2dfb4)
- fix: pass on `keep.all` when
  [`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md)
  receives keys
- [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md):
  Now reports % of partner connections kept across datasets.

**Full Changelog**:
<https://github.com/natverse/coconatfly/compare/v0.2.1>…v0.2.2

## coconatfly 0.2.1

- [`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md)
  gets a `MoreArgs` argument to pass on to subsidiary functions for each
  dataset
- Using this
  [`cf_cosine_plot()`](https://natverse.org/coconatfly/reference/cf_cosine_plot.md)
  and friends now prefer malecns foreign types (flywireType, mancType)
  when available and co-clustering with another dataset by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/27>
- [`cf_meta()`](https://natverse.org/coconatfly/reference/cf_meta.md):
  new `keep.all` argument keep all columns, not just shared columns by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/30>
- support for min_datasets in cosine clustering by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/31>

**Full Changelog**:
<https://github.com/natverse/coconatfly/compare/v0.2.0>…v0.2.1

## coconatfly 0.2.0

Although coconatfly has been in use for some time, this is the first
stable release. It corresponds to what was used in the recent
publication:

Schlegel, P., Yin, Y., Bates, A.S. et al.  Whole-brain annotation and
multi-connectome cell typing of Drosophila. Nature 634, 139–152 (2024).
<https://doi.org/10.1038/s41586-024-07686-5>

The release is motivated in part because of some forthcoming changes
that will slightly alter some core functionality of the package.

### What’s Changed

- Minimal support for FANC clustering by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/1>
- Feature/flywire examples by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/coconatfly/pull/5>
- First version cf_partner_summary by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/7>
- malevnc_meta -\> manc_meta by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/coconatfly/pull/10>
- fix handling of manc partners by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/11>
- Update test-triple.R by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/12>
- Support for the upcoming optic lobe release by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/14>
- Allow messier input keys by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/coconatfly/pull/15>
- cf_partner summary enh by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/16>
- flywire/fanc use different threshold definition by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/17>
- Add connectivity and basic metadata support for banc by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/19>
- Feature/fanc meta by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/21>
- Fix/banc optic meta by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/25>
- Feature/cosine plot warn on drop by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/coconatfly/pull/26>

**Full Changelog**:
<https://github.com/natverse/coconatfly/commits/v0.2.0>
