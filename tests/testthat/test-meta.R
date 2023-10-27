test_that("metadata", {

  expect_equal(
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012', flywire = 'rhubarb')),
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012'))
  )

  skip_if_not_installed('malevnc')
  expect_true(all(grepl("descending",
                        cf_meta(cf_ids(manc='DNa02'))$class)))

})
