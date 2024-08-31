test_that("vfb id mapping works", {
  skip_if_not_installed('vfbconnectr')
  dnm=cf_meta(cf_ids(manc='/type:DNa0[1-3]', hemibrain = '/type:DNa0[1-3]'))
  expect_s3_class(df <- cf_vfb_ids(dnm), 'data.frame')
  expect_equal(df$vfb_id, cf_vfb_ids(dnm$key))
  expect_equal(df$vfb_id,
               c("VFB_jrchjtfe", "VFB_jrchjtff", "VFB_jrchjtfg", "VFB_jrcv08an",
                 "VFB_jrcv08aw", "VFB_jrcv07ta", "VFB_jrcv07t2", "VFB_jrcv0kf2",
                 "VFB_jrcv0jtf"))
})
