test_that("cf_partners works", {
  expect_error(cf_partners(cf_ids(hemibrain = "")))

  expect_equal(nrow(cf_partners(cf_ids(hemibrain = '/DA2.*PN'), threshold = 10, partners = 'i')), 38)
})

test_that("cf_partner_summary works", {
  skip_if_not_installed('reticulate')
  expect_is(
    dnao <- cf_partner_summary(cf_ids("DNa02", datasets = c("banc", "fanc")), threshold = 10, partners = 'o'),
    'data.frame'
  )
  expect_true(any(grepl("w-cHIN", dnao$type.post)))
})
