test_that("cf_partners works", {
  expect_error(cf_partners(cf_ids(hemibrain = "")))

  expect_equal(nrow(cf_partners(cf_ids(hemibrain = '/DA2.*PN'), threshold = 10, partners = 'i')), 38)
})
