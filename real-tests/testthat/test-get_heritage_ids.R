test_that("get_heritage_ids() works with valid sf input", {
  skip_if_not_installed("sf")
  skip_on_cran()
  skip_on_ci()

  # Load the sf object from inst/extdata
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  skip_if_not(file.exists(sevres_path), "Missing test data: sevres.rda")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Try to call the function; skip if no data retrieved
  result <- get_heritage_ids(sevres, buffer = 500, verbose = FALSE)

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
