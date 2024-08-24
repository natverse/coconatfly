test_that("dr_coconatfly works", {
  expect_output(expect_message(r <- dr_coconatfly(), "Dataset details"), 'server')
  expect_s3_class(r, 'data.frame')
  # all datasets should be available unless we have an issue with the production
  # flywire style datasets
  skip_if_not_installed('fancr')
  skip_if_not_installed('reticulate')
  expect_true(!any(is.na(r$version)))
})
