test_that("geo_extent() computes bounding boxes correctly", {
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

  # Run function silently
  bb_sevres <- quiet(geo_extent(sevres))
  bb_hauts  <- quiet(geo_extent(hauts_de_seine, crs = 2154))

  # Check output structure
  expect_s3_class(bb_sevres, "data.frame")
  expect_s3_class(bb_hauts, "data.frame")

  expect_named(bb_sevres, c("left", "bottom", "right", "top"))
  expect_named(bb_hauts, c("left", "bottom", "right", "top"))

  # Check number of rows matches input features
  expect_equal(nrow(bb_sevres), nrow(sevres))
  expect_equal(nrow(bb_hauts), nrow(hauts_de_seine))

  # Check bounding box validity (min <= max)
  expect_true(all(bb_sevres$left <= bb_sevres$right))
  expect_true(all(bb_sevres$bottom <= bb_sevres$top))
  expect_true(all(bb_hauts$left <= bb_hauts$right))
  expect_true(all(bb_hauts$bottom <= bb_hauts$top))

  # Check CRS transformation consistency (global reference bbox)
  sevres_4326 <- sf::st_transform(sevres, 4326)
  bb_ref <- sf::st_bbox(sevres_4326)

  expect_true(all(bb_sevres$left >= bb_ref["xmin"]))
  expect_true(all(bb_sevres$right <= bb_ref["xmax"]))
  expect_true(all(bb_sevres$bottom >= bb_ref["ymin"]))
  expect_true(all(bb_sevres$top <= bb_ref["ymax"]))
})
