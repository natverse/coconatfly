test_that("dataset functions work", {
  expect_equal(match_datasets('flywire'), 'flywire')
  expect_equal(match_datasets('fly'), 'flywire')
  expect_error(match_datasets('f'))
  expect_equal(cf_datasets(),
               lengthen_datasets(abbreviate_datasets(cf_datasets())))

  expect_equal(
    abbreviate_datasets(c("flywire", "flywire", "hemibrain", "banc", "fanc", "manc")),
               c("fw", "fw", "hb", "bc", "fv", "mv"))

  coconat::register_dataset('rhubarb', shortname = 'rb',
                            species = 'Rheum rhabarbarum', sex='U', age='adult',
                            namespace = 'coconatfly')
  expect_equal(abbreviate_datasets("rhubarb"), "rb")
  expect_equal(lengthen_datasets("rb"), "rhubarb")
})
