test_that("use_superclass renames columns correctly", {
  meta <- cf_meta(cf_ids(hemibrain = "MBON01"))
  expect_true("class" %in% colnames(meta))
  expect_true("subclass" %in% colnames(meta))

  meta_super <- cf_meta(cf_ids(hemibrain = "MBON01"), use_superclass = TRUE)
  expect_true("superclass" %in% colnames(meta_super))
  expect_true("class" %in% colnames(meta_super))
  expect_false("subsubclass" %in% colnames(meta_super))
})

test_that("coconatfly.use_superclass option works", {
  old_opt <- getOption("coconatfly.use_superclass")
  on.exit(options(coconatfly.use_superclass = old_opt))

  options(coconatfly.use_superclass = TRUE)
  meta <- cf_meta(cf_ids(hemibrain = "MBON01"))
  expect_true("superclass" %in% colnames(meta))
})

test_that("top-level class values are harmonised to malecns style", {
  ds <- c(
    rep("flywire", 10),
    rep("manc", 8),
    "malecns",
    "flywire",
    "manc",
    "banc"
  )
  cls <- c(
    "ascending",
    "descending",
    "central",
    "sensory",
    "sensory_ascending",
    "visual_projection",
    "visual_centrifugal",
    "endocrine",
    "motor",
    "optic",  # flywire optic -> ol_intrinsic
    "ascending neuron",
    "descending neuron",
    "sensory neuron",
    "sensory ascending",
    "sensory descending",
    "efferent neuron",
    "efferent ascending",
    "intrinsic neuron",
    "descending_neuron",
    "not_a_known_class",
    "not_a_known_class",
    "Sensory TBD"
  )
  expect <- c(
    "ascending_neuron",
    "descending_neuron",
    "cb_intrinsic",
    "cb_sensory",
    "sensory_ascending",
    "visual_projection",
    "visual_centrifugal",
    "endocrine",
    "cb_motor",
    "ol_intrinsic",  # flywire optic -> ol_intrinsic
    "ascending_neuron",
    "descending_neuron",
    "vnc_sensory",
    "sensory_ascending",
    "sensory_descending",
    "vnc_efferent",
    "efferent_ascending",
    "vnc_intrinsic",
    "descending_neuron",
    "not_a_known_class",
    "not_a_known_class",
    "Sensory TBD"  # banc skipped for now, passed through unchanged
  )
  expect_equal(harmonise_top_class_values(cls, ds), expect)

  expect_equal(
    harmonise_top_class_values("not_a_known_class", "flywire", unknown_as_na = TRUE),
    NA_character_
  )

  expect_equal(
    harmonise_top_class_values("descending_neuron", "malecns", unknown_as_na = TRUE),
    "descending_neuron"
  )
})

test_that("metadata", {

  expect_warning(
    expect_equal(
      cf_meta(ids = cf_ids(hemibrain = 'AOTU012', flywire = 'rhubarb')),
      cf_meta(ids = cf_ids(hemibrain = 'AOTU012'))
    ),
    "No matching ids")

  expect_error(cf_meta(ids = cf_ids(hemibrain = 'AOTU012', expand = F)))

  skip_if_not_installed('malevnc')
  expect_true(all(grepl("descending",
                        cf_meta(cf_ids(manc='DNa02'))$class)))

  expect_s3_class(
    dna02meta <- cf_meta(cf_ids(hemibrain = 'DNa02', manc='DNa02')),
    'data.frame')
  expect_s3_class(
    dna02meta2 <- cf_meta(cf_ids(hemibrain = 'DNa02', manc='DNa02'), keep.all = T),
    'data.frame')
  expect_contains(colnames(dna02meta2), c("serial", "birthtime"))
})

test_that("fanc/banc ids/metadata", {
  skip_if_not_installed('fancr')
  skip_if_not_installed('reticulate')
  skip_if_not_installed('bancr', minimum_version = '0.2.1')
  # bancr must be registered for banc queries to work
  skip_if_not(tryCatch({suppressWarnings(bancr::register_banc_coconat()); TRUE}, error=function(e) FALSE),
              message = "bancr not properly configured")
  expect_warning(
    expect_null(cf_meta(cf_ids(banc='/rhubarb.+'))),
    "No matching ids")
})

test_that("extra datasets", {
  register_rhubarb()
  expect_true(is.data.frame(df <- cf_meta(cf_ids(rhubarb=10001:10003))))
  expect_equal(cf_meta(cf_ids(rhubarb=10001)), df[1,,drop=FALSE])

  expect_true(is.data.frame(cf_meta(cf_ids(rhubarb=10001, flywire='DNa02'))))

  # badrhubarb has no idfun, so cf_ids errors when trying to expand (the new default)
  expect_error(cf_ids(badrhubarb=10001))
})
