test_that("triple_cosine_plot works", {
  fcdv <- try(fafbseg::flywire_connectome_data_version(), silent = T)
  testthat::skip_if(inherits(fcdv, "try-error"))

  expect_s3_class(
    hc <- triple_cosine_plot('/type:AOTU063.*', partners = 'o', heatmap = F),
            'hclust')

  bl=list(labels = c("hb:800929667", "hb:791039731", "fw:720575940620326253",
                     "fw:720575940621925631", "fw:720575940631129362",
                     "fw:720575940618697118"),
          order = c(1L, 3L, 5L, 2L, 4L, 6L))

  expect_equal(hc[c("labels", "order")],bl)
})
