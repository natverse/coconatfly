# coconatfly (development version)

# coconatfly 0.2.1

* `cf_meta()` gets a `MoreArgs` argument to pass on to subsidiary functions for 
  each dataset
* Using this `cf_cosine_plot()` and friends now prefer malecns foreign types 
  (flywireType, mancType) when available and co-clustering with another dataset
  by @jefferis in https://github.com/natverse/coconatfly/pull/27
* `cf_meta()`: new `keep.all` argument keep all columns, not just shared columns
  by @jefferis in https://github.com/natverse/coconatfly/pull/30
* support for min_datasets in cosine clustering 
  by @jefferis in https://github.com/natverse/coconatfly/pull/31

**Full Changelog**: https://github.com/natverse/coconatfly/compare/v0.2.0...v0.2.1


# coconatfly 0.2.0

Although coconatfly has been in use for some time, this is the first stable 
release. It corresponds to what was used in the recent publication:

Schlegel, P., Yin, Y., Bates, A.S. et al. 
Whole-brain annotation and multi-connectome cell typing of Drosophila. 
Nature 634, 139–152 (2024). https://doi.org/10.1038/s41586-024-07686-5

The release is motivated in part because of some forthcoming changes that will
slightly alter some core functionality of the package.

## What's Changed
* Minimal support for FANC clustering by @jefferis in https://github.com/natverse/coconatfly/pull/1
* Feature/flywire examples by @jefferis in https://github.com/natverse/coconatfly/pull/5
* First version cf_partner_summary by @jefferis in https://github.com/natverse/coconatfly/pull/7
* malevnc_meta -> manc_meta by @jefferis in https://github.com/natverse/coconatfly/pull/10
* fix handling of manc partners by @jefferis in https://github.com/natverse/coconatfly/pull/11
* Update test-triple.R by @jefferis in https://github.com/natverse/coconatfly/pull/12
* Support for the upcoming optic lobe release by @jefferis in https://github.com/natverse/coconatfly/pull/14
* Allow messier input keys by @jefferis in https://github.com/natverse/coconatfly/pull/15
* cf_partner summary enh by @jefferis in https://github.com/natverse/coconatfly/pull/16
* flywire/fanc use different threshold definition by @jefferis in https://github.com/natverse/coconatfly/pull/17
* Add connectivity and basic metadata support for banc by @jefferis in https://github.com/natverse/coconatfly/pull/19
* Feature/fanc meta by @jefferis in https://github.com/natverse/coconatfly/pull/21
* Fix/banc optic meta by @jefferis in https://github.com/natverse/coconatfly/pull/25
* Feature/cosine plot warn on drop by @jefferis in https://github.com/natverse/coconatfly/pull/26


**Full Changelog**: https://github.com/natverse/coconatfly/commits/v0.2.0
