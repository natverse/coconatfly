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
