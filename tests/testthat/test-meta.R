test_that("keys2df", {
  df=data.frame(id = c("12345", "12345"),
                dataset = c("flywire", "hemibrain"))
  expect_equal(keys2df(c("fw:12345", "hb:12345")), df)
})
