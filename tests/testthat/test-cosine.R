test_that("multiplication works", {
  expect_s3_class(
    cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), group = F, heatmap = F),
    'hclust')

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
})
