test_that("dr_coconatfly works", {
  expect_output(expect_message(r <- dr_coconatfly(), "Dataset details"), 'server')
  expect_s3_class(r, 'data.frame')
  # all datasets should be available unless we have an issue with the production
  # flywire style datasets
  skip_if_not_installed('fancr')
  skip_if_not_installed('reticulate')

  if(any(is.na(r$version))) {
    cfc=cf_connections()
    print(cfc)
  }
})

test_that("fix_mixed_col_types converts mixed-type id cols to character without leaking values", {

  l <- list(
    a = tibble::tibble(
      id = 1:3,                    # non-character
      already_chr = c("x", "y", "z")# character (must remain unchanged)
    ),
    b = tibble::tibble(
      id = c("4", "5", "6"),       # character -> forces 'id' to be treated as mixed
      already_chr = c("u", "v", "w")
    )
  )

  out <- fix_mixed_col_types(l)

  # id becomes character in both
  expect_type(out$a$id, "character")
  expect_type(out$b$id, "character")
  expect_equal(out$a$id, c("1", "2", "3"))
  expect_equal(out$b$id, c("4", "5", "6"))

  # character columns must not be changed/overwritten
  expect_equal(out$a$already_chr, c("x", "y", "z"))
  expect_equal(out$b$already_chr, c("u", "v", "w"))

  # 2) A second mixed column to guard against cross-column contamination
  l2 <- list(
    a = tibble::tibble(id = 1:2, other = 10:11),
    b = tibble::tibble(id = c("1", "2"), other = c("10", "11"))
  )
  out2 <- fix_mixed_col_types(l2)

  expect_type(out2$a$id, "character")
  expect_type(out2$a$other, "character")
  expect_equal(out2$a$other, c("10", "11"))
})
