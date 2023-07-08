test_that("key handling works", {

  df=data.frame(id = c("12345", "12345"),
                dataset = c("flywire", "hemibrain"))
  expect_equal(keys2df(c("fw:12345", "hb:12345")), df)

  idlist=list(flywire='4611686018427387904', hemibrain='12345')
  keyvec=c("fw:4611686018427387904", "hb:12345")
  keydf=data.frame(id = c("4611686018427387904", "12345"),
                   dataset = c("flywire", "hemibrain"))
  expect_equal(keys(idlist), keyvec)
  expect_equal(keys(keydf), keyvec)
  expect_equal(keys2df(keyvec), keydf)
  expect_equal(keys2list(keyvec), idlist)

  expect_equal(cf_ids(hemibrain = '/MBON0[12].*', expand = T),
               cf_ids(hemibrain = c("612371421", "673509195", "424789697", "5813022341"))
               )

  expect_warning(cf_ids(hemibrain = 'rhubarb', expand = T))

  expect_equal(c(
    cf_ids(hemibrain = '/MBON0[12].*', flywire=1:3),
    cf_ids(hemibrain = '612371421', flywire=3:5)),
    cf_ids(hemibrain = c("612371421", "673509195", "424789697", "5813022341"),
         flywire=as.character(1:5)))
})
