test_that("key handling works", {

  df=data.frame(id = c("12345", "12345"),
                dataset = c("flywire", "hemibrain"))
  expect_equal(keys2df(c("fw:12345", "hb:12345")), df)


  idlist=list(flywire='4611686018427387904', hemibrain='12345')
  keyvec=c("fw:4611686018427387904", "hb:12345")

  expect_equal(keys(keyvec), keyvec)
  expect_equal(keys(paste(keyvec, collapse = ',')), keyvec)
  expect_equal(keys(paste(keyvec, collapse = '\t')), keyvec)
  # messy whitespace/separators
  expect_equal(keys(paste(' ', keyvec, collapse = ',', " ")), keyvec)
  expect_equal(keys(paste0(" ", keyvec[1], " ")), keyvec[1])

  keydf=data.frame(id = c("4611686018427387904", "12345"),
                   dataset = c("flywire", "hemibrain"))
  expect_equal(keys(idlist), keyvec)
  expect_equal(keys(keydf), keyvec)
  expect_equal(keys2df(keyvec), keydf)
  expect_equal(keys2df(paste(' ', keyvec, collapse = ',', " ")), keydf)
  expect_equal(keys2list(keyvec), idlist)
  expect_error(keys('4611686018427387904 12345'))

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

test_that("fanc/banc ids/metadata", {
  skip_if_not_installed('fancr')
  skip_if_not_installed('reticulate')
  expect_in(
    cf_ids(fanc='type:DNa01', expand = TRUE)$fanc,
    fancr::fanc_latestid(c("648518346488820970", "648518346475464576"),
                         version='latest'))

  expect_length(dna02keys <- cf_ids(banc='/DNa02', keys = T), 2L)
  expect_warning(
    expect_in(cf_ids(banc='DNa02', keys = T), dna02keys))
})
