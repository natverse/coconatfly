test_that("dr_coconatfly works", {
  expect_output(expect_message(r <- dr_coconatfly(), "Dataset details"), 'server')
  expect_s3_class(r, 'data.frame')
  expect_true(!any(is.na(r$version)))
})
