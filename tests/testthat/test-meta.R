test_that("idvec2iddf", {
  df=data.frame(id = c("12345", "12345"),
                dataset = c("flywire", "hemibrain"))
  expect_equal(idvec2iddf(c("fw:12345", "hb:12345")), df)
})
