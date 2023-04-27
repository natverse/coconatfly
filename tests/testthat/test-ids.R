test_that("key handling works", {
  idlist=list(flywire='4611686018427387904', hemibrain='12345')
  keyvec=c("fw:4611686018427387904", "hb:12345")
  keydf=data.frame(id = c("4611686018427387904", "12345"),
                   dataset = c("flywire", "hemibrain"))
  expect_equal(keys(idlist), keyvec)
  expect_equal(keys(keydf), keyvec)
  expect_equal(keys2df(keyvec), keydf)
  expect_equal(keys2list(keyvec), idlist)
})
