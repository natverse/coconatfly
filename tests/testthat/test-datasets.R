test_that("dataset functions work", {
  expect_equal(match_datasets('flywire'), 'flywire')
  expect_equal(match_datasets('fly'), 'flywire')
  expect_error(match_datasets('f'))
  expect_equal(cf_datasets(),
               lengthen_datasets(abbreviate_datasets(cf_datasets())))

  expect_equal(
    abbreviate_datasets(c("flywire", "flywire", "hemibrain", "banc", "fanc", "manc")),
               c("fw", "fw", "hb", "bc", "fv", "mv"))

  register_rhubarb()
  expect_equal(abbreviate_datasets("rhubarb"), "rb")
  expect_equal(lengthen_datasets("rb"), "rhubarb")
  # check that builtin datasets agree
  cfids.datasets=eval(formals(cf_ids)$datasets)
  expect_true(all(cf_datasets('builtin') %in% cfids.datasets))
  expect_true(all(
    cfids.datasets %in% union(cf_datasets('builtin'), c("brain", "vnc") )))
})
