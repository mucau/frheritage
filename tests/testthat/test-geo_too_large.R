test_that("geo_too_large() checks sf object size correctly", {
  skip_if_not_installed("sf")

  # Load test data from package
  sevres_path <- system.file("extdata", "sevres.rda", package = "frheritage")
  hauts_path <- system.file("extdata", "hauts_de_seine.rda", package = "frheritage")
  skip_if(!file.exists(sevres_path), "sevres.rda not found")
  skip_if(!file.exists(hauts_path), "hauts_de_seine.rda not found")
  load(sevres_path)
  load(hauts_path)

  # Ensure objects are sf
  expect_s3_class(sevres, "sf")
  expect_s3_class(hauts_de_seine, "sf")

  # Test: run silently on normal-sized objects
  expect_message(
    geo_too_large(sevres, area_threshold = 1e12, extent_threshold = 1e6),
    "within acceptable size limits"
  )
  expect_message(
    geo_too_large(hauts_de_seine, area_threshold = 1e12, extent_threshold = 1e6),
    "within acceptable size limits"
  )

  # Test: trigger error by lowering thresholds
  expect_error(
    geo_too_large(sevres, area_threshold = 1, extent_threshold = 1),
    regexp = "Spatial object too large"
  )
  expect_error(
    geo_too_large(hauts_de_seine, area_threshold = 1, extent_threshold = 1),
    regexp = "Spatial object too large"
  )

  # Test: input must be sf
  not_sf <- data.frame(x = 1:2, y = 3:4)
  expect_error(geo_too_large(not_sf), "must be an sf object")

  # Test: invisible return FALSE when within limits
  expect_false(geo_too_large(sevres, area_threshold = 1e12, extent_threshold = 1e6, verbose = FALSE))
})
