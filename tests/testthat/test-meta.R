test_that("metadata", {

  expect_equal(
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012', flywire = 'rhubarb')),
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012'))
  )
})
