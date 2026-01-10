test_that("geo_prepare() prepares and aggregates sf objects correctly", {
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

  # Run function silently on both datasets
  output <- capture.output({
    res_sevres <- quiet(geo_prepare(sevres, crs = 2154, buffer = 50))
    res_hauts  <- quiet(geo_prepare(hauts_de_seine, crs = 2154, buffer = 50))
  })
  expect_length(output, 0)

  # Returned objects are sf
  expect_s3_class(res_sevres, "sf")
  expect_s3_class(res_hauts, "sf")

  # Geometry type is POLYGON after aggregation
  expect_true(all(sf::st_geometry_type(res_sevres) == "POLYGON"))
  expect_true(all(sf::st_geometry_type(res_hauts) == "POLYGON"))

  # Geometries are non-empty
  expect_false(any(sf::st_is_empty(res_sevres)))
  expect_false(any(sf::st_is_empty(res_hauts)))

  # CRS is correctly set
  expect_equal(sf::st_crs(res_sevres)$epsg, 2154)
  expect_equal(sf::st_crs(res_hauts)$epsg, 2154)

  # Aggregated geometries should reduce the number of features
  expect_true(nrow(res_sevres) <= nrow(sevres))
  expect_true(nrow(res_hauts) <= nrow(hauts_de_seine))
})
