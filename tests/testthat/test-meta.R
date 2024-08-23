test_that("metadata", {

  expect_equal(
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012', flywire = 'rhubarb')),
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012'))
  )

  skip_if_not_installed('malevnc')
  expect_true(all(grepl("descending",
                        cf_meta(cf_ids(manc='DNa02'))$class)))

})

test_that("fanc/banc metadata", {

  expect_in(
    cf_ids(fanc='type:DNa01', expand = TRUE)$fanc,
    fancr::fanc_latestid(c("648518346488820970", "648518346475464576")))

})
