test_that("dataset functions work", {
  expect_equal(match_datasets('flywire'), 'flywire')
  expect_equal(match_datasets('fly'), 'flywire')
  expect_error(match_datasets('f'))
  expect_equal(cf_datasets(),
               lengthen_datasets(abbreviate_datasets(cf_datasets())))
})

test_that("match_datasets suggests close matches", {
  # Typo should suggest correct dataset
  expect_error(match_datasets("hemibran"), "Did you mean 'hemibrain'")
  expect_error(match_datasets("flywie"), "Did you mean 'flywire'")
})

test_that("match_datasets shows no suggestion for unrelated names", {
  # Unrelated name should NOT suggest anything
  expect_error(match_datasets("aedes"), regexp = "Unknown dataset.*'aedes'")
  expect_error(match_datasets("aedes"), regexp = "register_dataset")
  # Verify no "Did you mean" for distant matches
  err <- tryCatch(match_datasets("aedes"), error = function(e) conditionMessage(e))
  expect_false(grepl("Did you mean", err))
})

test_that("match_datasets mentions manual registration", {
  expect_error(match_datasets("foobar"), "registered manually")

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
