test_that("cosine plots work", {
  expect_s3_class(
    cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), group = F, heatmap = F),
    'hclust')

  expect_true(
    is.matrix(cf_cosine_plot(
      cf_ids(hemibrain="/type:LAL00.+"), group = F, heatmap = F, matrix = T)))

  lalmeta=cf_meta(cf_ids(hemibrain="/type:LAL00.+"))
  # NB left_join requires the id columns to have the same character data type
  mytypes=data.frame(
    id=as.character(c(5813047453, 1011611587)),
    mytype=c("alice", 'bob'))
  lalmeta2=dplyr::left_join(lalmeta, mytypes, by='id') %>%
    mutate(label=glue::glue('{type}_{side} :: {mytype}'))

  expect_type(
    dend <- lalmeta2 %>% with(cf_cosine_plot(key, labRow=label, heatmap = T)),
    'list')
  expect_error(cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), letters[1:15]))
  op <- options(fafbseg.use_static_celltypes=T)
  on.exit(options(op))
  nofw=inherits(try(dna02.fw <- cf_ids(flywire = 'DNa02', expand = T), silent = TRUE), "try-error")
  skip_if(nofw, message = 'No flywire annotations available.')

  lalids=cf_ids(hemibrain="/type:LAL(00.+|044)", flywire = "/type:LAL00.+")
  suppressWarnings(suppressMessages(hc <- cf_cosine_plot(lalids, heatmap = F)))
  suppressWarnings(suppressMessages(expect_message(hc1 <- cf_cosine_plot(lalids, min_datasets = -1, heatmap = F),
                 "Keeping .*connections")))
  suppressWarnings(
    suppressMessages(
      expect_equal(cf_cosine_plot(lalids, min_datasets = 1, heatmap = F),
                   hc1)))
})


test_that("cosine plot with no partners", {
  op <- options(fafbseg.use_static_celltypes=T)
  on.exit(options(op))

  suppressWarnings(
    suppressMessages(
      expect_warning(cf_cosine_plot(cf_ids(hemibrain = 'ORN_DA1', flywire = 'ORN_DA1'), threshold = 8),
                     "no inputs.*flywire")
    ))
})


test_that("cosine plot with v2 flywire data", {
  skip_if(inherits(register_flywire2(), 'try-error'))
  op <- options(fafbseg.use_static_celltypes=T)
  on.exit(options(op))
  cf_cosine_plot(cf_ids('/type:LAL0(08|09|10|42)', datasets = c("hemibrain", "fx")))
})
