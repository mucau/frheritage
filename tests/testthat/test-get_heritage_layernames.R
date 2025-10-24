test_that("get_heritage_layernames() returns the full table when no code is provided", {
  df <- get_heritage_layernames()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("code", "label") %in% colnames(df)))
  expect_true(nrow(df) >= 1)
})

test_that("get_heritage_layernames() returns only requested valid codes", {
  df <- get_heritage_layernames(c("IMDN", "IMMH"))
  expect_s3_class(df, "data.frame")
  expect_equal(df$code, c("IMDN", "IMMH"))
})

test_that("get_heritage_layernames() errors on invalid code", {
  expect_error(get_heritage_layernames("INVALID"), regexp = "Invalid code")
})

test_that("get_heritage_layernames() errors when part of codes are invalid", {
  expect_error(get_heritage_layernames(c("IMDN", "INVALID")), regexp = "Invalid code")
})
