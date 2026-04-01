test_that("cf_partners works", {
  expect_error(cf_partners(cf_ids(hemibrain = "")))

  expect_equal(nrow(cf_partners(cf_ids(hemibrain = '/DA2.*PN'), threshold = 10, partners = 'i')), 38)
})

test_that("cf_partner_summary works", {
  skip_if_not_installed('reticulate')
  expect_s3_class(
    dnao <- cf_partner_summary(cf_ids(fanc="/DNa02"), threshold = 10, partners = 'o'),
    'data.frame'
  )
  expect_true(any(grepl("w-cHIN", dnao$type.post)))
})

test_that("cf_partner_summary works", {
  skip_if_not_installed('malecns')
  expect_s3_class(
    dnao <- cf_partner_summary(cf_ids(malecns="/DNa02",), threshold = 10, partners = 'o'),
    'data.frame'
  )
  expect_true(any(grepl("w-cHIN", dnao$type.post)))
})

test_that("cf_partner_summary works", {

  expect_s3_class(
    dnao <- cf_partner_summary(cf_ids(opticlobe = 'LLPC1'), threshold = 10, partners = 'o'),
    'data.frame'
  )
  expect_true(any(grepl("HSS", dnao$type.post)))

  skip_if_not_installed('malecns')
  expect_s3_class(
    dnao <- cf_partner_summary(cf_ids(malecns="/DNa02"), threshold = 10, partners = 'o'),
    'data.frame'
  )
  expect_true(any(grepl("w-cHIN", dnao$type.post)))
})

test_that("partner metadata enrichment works", {
  # Test that partnerfuns returning minimal data (pre_id, post_id, weight)
  # get enriched with metadata from cf_meta automatically
  register_rhubarb()

  # cf_partners should add metadata columns even though .rhubarb_partners
  # only returns pre_id, post_id, weight
  pp <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs", threshold = 1)

  expect_s3_class(pp, "data.frame")
  expect_true(nrow(pp) > 0)

  # These columns come from add_partner_metadata via cf_meta
  expect_true("type" %in% colnames(pp))
  expect_true("class" %in% colnames(pp))

  # Verify the types are correct (partners of neuron 10001 are neurons 10002 and 10003)
  expect_true(all(c("G1.2_PN", "G2_PN") %in% pp$type))
})


test_that("cf_partners details='neither' returns minimal columns", {
  register_rhubarb()

  # With details="neither", should NOT have type/class columns
  pp <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                    threshold = 1, details = "neither")

  expect_s3_class(pp, "data.frame")
  expect_true(nrow(pp) > 0)

  # Should have minimal columns
  expect_true(all(c("pre_id", "post_id", "weight", "pre_key", "post_key", "dataset")
                  %in% colnames(pp)))
  # Should NOT have metadata columns
  expect_false("type" %in% colnames(pp))
  expect_false("class" %in% colnames(pp))
})


test_that("cf_partners details='query' adds metadata to query neurons", {
  register_rhubarb()

  # For outputs: query is pre, partner is post
  pp <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                    threshold = 1, details = "query")

  expect_s3_class(pp, "data.frame")
  expect_true(nrow(pp) > 0)

  # Should have type (from query neuron 10001)
  expect_true("type" %in% colnames(pp))
  expect_equal(unique(pp$type), "G1_PN")  # neuron 10001's type
})


test_that("cf_partners details='both' adds metadata to both sides", {
  register_rhubarb()

  pp <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                    threshold = 1, details = "both")

  expect_s3_class(pp, "data.frame")
  expect_true(nrow(pp) > 0)

  # Should have suffixed columns for both sides
  expect_true("type.pre" %in% colnames(pp))
  expect_true("type.post" %in% colnames(pp))
  expect_true("class.pre" %in% colnames(pp))
  expect_true("class.post" %in% colnames(pp))

  # Query neuron 10001 is pre (for outputs)
  expect_equal(unique(pp$type.pre), "G1_PN")
  # Partners are 10002 and 10003
  expect_true(all(c("G1.2_PN", "G2_PN") %in% pp$type.post))
})


test_that("cf_add_meta adds metadata to single key column", {
  register_rhubarb()

  # Get partners without details
  partners <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                          threshold = 1, details = "neither")

  # Add metadata to post_key column
  result <- cf_add_meta(partners, keycol = "post_key")

  expect_true("type" %in% names(result))
  expect_true("class" %in% names(result))
  # Check that metadata was actually joined
  expect_true(all(c("G1.2_PN", "G2_PN") %in% result$type))
})


test_that("cf_add_meta handles multiple key columns with suffixes", {
  register_rhubarb()

  partners <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                          threshold = 1, details = "neither")

  result <- cf_add_meta(partners,
                        keycol = c("pre_key", "post_key"),
                        suffix = c(".pre", ".post"))

  expect_true("type.pre" %in% names(result))
  expect_true("type.post" %in% names(result))
  expect_true("class.pre" %in% names(result))
  expect_true("class.post" %in% names(result))
  expect_equal(unique(result$type.pre), "G1_PN")
  expect_true(all(c("G1.2_PN", "G2_PN") %in% result$type.post))
})


test_that("cf_add_meta uses dataset-encoded keys without requiring dataset column", {
  register_rhubarb()

  partners <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                          threshold = 1, details = "neither")
  partners$dataset <- NULL

  result <- cf_add_meta(partners, keycol = "post_key")

  expect_true("type" %in% names(result))
  expect_true(all(c("G1.2_PN", "G2_PN") %in% result$type))
})


test_that("cf_add_meta cols parameter limits columns", {
  register_rhubarb()

  partners <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                          threshold = 1, details = "neither")

  result <- cf_add_meta(partners, keycol = "post_key",
                        cols = c("type", "side"))

  expect_true("type" %in% names(result))
  expect_true("side" %in% names(result))
  expect_false("class" %in% names(result))
  expect_false("subclass" %in% names(result))
})


test_that("cf_add_meta warns about missing columns", {
  register_rhubarb()

  partners <- cf_partners(cf_ids(rhubarb = 10001), partners = "outputs",
                          threshold = 1, details = "neither")

  # Character vector with non-existent column should warn
  expect_warning(
    cf_add_meta(partners, keycol = "post_key",
                cols = c("type", "nonexistent_col")),
    "not found in metadata"
  )
})
