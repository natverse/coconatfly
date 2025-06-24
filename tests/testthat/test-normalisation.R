test_that("field translation works", {
  cf=c("subclass", "class", "type", "rhubarb")
  fw=c("cell_class", "super_class", "cell_type", "rhubarb")
  expect_equal(translate_fields(fw, dataset = 'fly', direction = 'in'), cf)
  expect_equal(translate_fields(cf, dataset = 'fly', direction = 'out'), fw)

  l=as.list(letters[seq_along(cf)])
  names(l)=cf
  df=data.frame(l)
  expect_type(tofw <- field_translater(dataset = "flywire", direction = "out"), 'closure')
  df2=dplyr::rename_with(df, .fn=tofw)
  expect_equal(colnames(df2), fw)
})
