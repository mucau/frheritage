test_that("get_heritage_ids() works with valid sf input", {
  skip_if_not_installed("httptest2")
  skip_if_not_installed("sf")

  # Load the sf object from inst/extdata
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Mock HTTP responses
  httptest2::with_mock_dir("mock_get_heritage_ids", {
    result <- get_heritage_ids(sevres, buffer = 500, verbose = FALSE)
  })

  # Structure checks
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "title", "code") %in% names(result)))

  # Content checks
  expect_gt(nrow(result), 0)
  expect_true(all(nchar(result$id) > 0))
  expect_true(all(nchar(result$title) > 0))
  expect_true(all(!is.na(result$code)))

  # Uniqueness check
  expect_equal(nrow(result), length(unique(result$id)))
})
