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

test_that("side values are normalised to L/R/M/NA", {
  expect_equal(normalise_side(c("left", "L", "l", "LEFT")), c("L", "L", "L", "L"))
  expect_equal(normalise_side(c("right", "R", "r", "RIGHT")), c("R", "R", "R", "R"))
  expect_equal(normalise_side(c("midline", "M", "m", "center", "centre")),
               c("M", "M", "M", "M", "M"))
  expect_equal(normalise_side(c(NA, "", "unknown")),
               c(NA_character_, NA_character_, NA_character_))
})

test_that("tissue column is based on dataset", {
  expect_equal(dataset_tissue("flywire"), "brain")
  expect_equal(dataset_tissue("hemibrain"), "brain")
  expect_equal(dataset_tissue("opticlobe"), "brain")
  expect_equal(dataset_tissue("manc"), "vnc")
  expect_equal(dataset_tissue("fanc"), "vnc")
  expect_equal(dataset_tissue("yakubavnc"), "vnc")
  expect_equal(dataset_tissue("malecns"), "cns")
  expect_equal(dataset_tissue("banc"), "cns")
  expect_equal(dataset_tissue("unknown"), NA_character_)
})

test_that("sex column comes from dataset registration", {
  expect_equal(dataset_sex("hemibrain"), "F")
  expect_equal(dataset_sex("malecns"), "M")
  expect_equal(dataset_sex("manc"), "M")
})

test_that("cf_meta includes tissue, sex, and normalised side", {
  meta <- cf_meta(cf_ids(hemibrain = "MBON01"))
  expect_true("tissue" %in% colnames(meta))
  expect_true("sex" %in% colnames(meta))
  expect_true("side" %in% colnames(meta))
  expect_equal(unique(meta$tissue), "brain")
  expect_equal(unique(meta$sex), "F")
  # hemibrain MBON01 should have R/L sides
  expect_true(all(meta$side %in% c("L", "R", "M", NA_character_)))
})

test_that("coconatfly.harmonise_class option works", {
  skip_if_not_installed('malevnc')
  old_opt <- getOption("coconatfly.harmonise_class")
  on.exit(options(coconatfly.harmonise_class = old_opt))

  # Without harmonisation, manc and malecns have different class values
  options(coconatfly.harmonise_class = FALSE)
  meta_off <- cf_meta(cf_ids(manc = "AN07B004", malecns = "AN07B004"))
  manc_class <- unique(meta_off$class[meta_off$dataset == "manc"])
  malecns_class <- unique(meta_off$class[meta_off$dataset == "malecns"])
  # manc uses "ascending neuron", malecns uses "ascending_neuron"
  expect_false(identical(manc_class, malecns_class))

  # With harmonisation enabled, classes should match
  options(coconatfly.harmonise_class = TRUE)
  meta_on <- cf_meta(cf_ids(manc = "AN07B004", malecns = "AN07B004"))
  manc_class_h <- unique(meta_on$class[meta_on$dataset == "manc"])
  malecns_class_h <- unique(meta_on$class[meta_on$dataset == "malecns"])
  expect_equal(manc_class_h, malecns_class_h)
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
