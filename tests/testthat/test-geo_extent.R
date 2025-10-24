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

  # Test: run silently
  output <- capture.output({
    bb_sevres <- silent_run(geo_extent(sevres))
    bb_hauts <- silent_run(geo_extent(hauts_de_seine, crs = 2154))
  })
  expect_length(output, 0)

  # Test: returned objects are data.frames with correct columns
  expect_s3_class(bb_sevres, "data.frame")
  expect_s3_class(bb_hauts, "data.frame")
  expect_named(bb_sevres, c("left", "bottom", "right", "top"))
  expect_named(bb_hauts, c("left", "bottom", "right", "top"))

  # Test: bounding box values are consistent
  expect_true(all(bb_sevres$left <= bb_sevres$right))
  expect_true(all(bb_sevres$bottom <= bb_sevres$top))
  expect_true(all(bb_hauts$left <= bb_hauts$right))
  expect_true(all(bb_hauts$bottom <= bb_hauts$top))

  # Test: CRS transformation works
  sevres_trans <- st_transform(sevres, 4326)
  bb_trans <- geo_extent(sevres, crs = 4326)
  expect_true(all(bb_trans$left >= min(st_bbox(sevres_trans)["xmin"])))
  expect_true(all(bb_trans$bottom >= min(st_bbox(sevres_trans)["ymin"])))
})
