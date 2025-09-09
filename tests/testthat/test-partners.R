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
