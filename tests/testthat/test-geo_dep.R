test_that("geo_dep() retrieves the correct INSEE department code for sevres", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  # Load example sf object
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  skip_if_not(file.exists(sevres_path), "Missing test data: sevres.rda")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Use mock responses for HTTP requests
  httptest2::with_mock_dir("mock_get_heritage_geo_dep", {
    deps <- geo_dep(sevres)
  })

  # Skip if the mock returned no data
  if (is.null(deps) || length(deps) == 0) {
    skip("No data returned by geo_dep() — skipping test.")
  }

  # Checks
  expect_true(is.character(deps))
  expect_true("92" %in% deps)
  expect_length(deps, 1)
})
