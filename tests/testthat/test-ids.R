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

  expect_equal(res <- c(
    cf_ids(hemibrain = '/MBON0[12].*', flywire=1:3),
    cf_ids(hemibrain = '612371421', flywire=3:5)),
    cf_ids(hemibrain = c("612371421", "673509195", "424789697", "5813022341"),
         flywire=as.character(1:5)))

  expect_output(print(res), regexp = 'flywire.*hemibrain')

  expect_equal(keys(cf_ids(hemibrain = '/MBON01')), cf_ids(hemibrain = '/MBON01', keys = T))

  expect_warning(
    expect_equal(cf_ids(hemibrain = "/rhubarb", keys = T), character()),
    "No matching ids")
})

test_that("fanc/banc ids/metadata", {
  skip_if_not_installed('fancr')
  skip_if_not_installed('reticulate')
  expect_in(
    suppressWarnings(cf_ids(fanc='/type:DNa01', expand = TRUE)$fanc),
    fancr::fanc_latestid(c("648518346488820970", "648518346475464576"),
                         version='latest'))

  skip_if_not_installed('bancr')
  skip_if(inherits(suppressWarnings(bancr::register_banc_coconat()), 'try-error'))
  expect_length(dna02keys <- suppressWarnings(cf_ids(banc='/DNa02', keys = T)), 2L)
  expect_warning(
    expect_in(cf_ids(banc='DNa02', keys = T), dna02keys))
  expect_warning(
    expect_equal(lengths(cf_ids(banc='/rhubarb.+', expand = TRUE)), 0L, ignore_attr=TRUE),
               "No matching ids")
})

test_that("extra datasets", {
  register_rhubarb()
  expect_equal(rhu <- cf_ids(1, datasets = 'rhubarb'), list(rhubarb=1), ignore_attr = TRUE)
  expect_equal(cf_ids(1, datasets = 'rhubar'), rhu)
  skip_if_not_installed('malecns')
  skip_if_not_installed('bancr')
  expect_warning(
    expect_equal(length(cf_ids(1, datasets = c("brain", 'rhubar'))), 5L),
    "unable to map")

  expect_equal(rhu2 <- cf_ids(rhubarb=1:3), list(rhubarb=1:3), ignore_attr = TRUE)
  expect_equal(cf_ids(rhubar=1:3), rhu2)
  expect_equal(rhu2 <- cf_ids(rhubarb=1:3),
               list(rhubarb=1:3), ignore_attr = TRUE)
})
