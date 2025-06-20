test_that("metadata", {

  expect_equal(
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012', flywire = 'rhubarb')),
    cf_meta(ids = cf_ids(hemibrain = 'AOTU012'))
  )

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
  expect_null(cf_meta(cf_ids(banc='/rhubarb.+')))
})

test_that("extra datasets", {
  register_rhubarb()
  expect_true(is.data.frame(df <- cf_meta(cf_ids(rhubarb=1:3))))
  expect_equal(cf_meta(cf_ids(rhubarb=1)), df[1,,drop=FALSE])

  expect_true(is.data.frame(cf_meta(cf_ids(rhubarb=1, flywire='DNa02'))))

  expect_error(cf_meta(cf_ids(badrhubarb=1)), regexp = 'no metadata function')
})


