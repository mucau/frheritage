test_that("geo_dep() retrieves the correct INSEE department code for sevres", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  # Load example sf object
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  skip_if_not(file.exists(sevres_path), "Missing test data: sevres.rda")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Use mock responses for HTTP requests
  deps <- silent_run(geo_dep(sevres))

  # Checks
  expect_true(is.character(deps))
  expect_true("92" %in% deps)
  expect_length(deps, 1)
})
