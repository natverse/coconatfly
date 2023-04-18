test_that("dataset functions work", {
  expect_equal(match_datasets('flywire'), 'flywire')
  expect_equal(match_datasets('fly'), 'flywire')
  expect_error(match_datasets('f'))
  expect_equal(cf_datasets(),
               lengthen_datasets(abbreviate_datasets(cf_datasets())))
})
