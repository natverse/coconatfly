test_that("multiplication works", {
  expect_s3_class(
    cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), group = F, heatmap = F),
    'hclust')

  expect_s3_class(
    cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), group = T, heatmap = F),
    'hclust')
})
