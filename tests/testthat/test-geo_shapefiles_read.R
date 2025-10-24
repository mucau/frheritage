test_that("geo_shapefiles_read() works on file4d4443dc65a5.zip", {
  skip_if_not_installed("sf")

  # Path to the ZIP file
  zip_path <- system.file("extdata", "file4d4443dc65a5.zip", package = "frheritage")
  expect_true(file.exists(zip_path))

  # Read shapefiles
  sf_list <- geo_shapefiles_read(zip_path, crs = 2154)

  # Basic checks
  expect_type(sf_list, "list")
  expect_true(length(sf_list) > 0)  # Ensure at least one shapefile was read

  # All elements are sf objects
  expect_true(all(vapply(sf_list, inherits, logical(1), what = "sf")))

  # All sf objects have at least one row
  expect_true(all(vapply(sf_list, function(x) nrow(x) > 0, logical(1))))

  # All sf objects are in EPSG:2151
  expect_true(all(vapply(sf_list, function(x) sf::st_crs(x)$epsg == 2154, logical(1))))
})

test_that("geo_shapefiles_read() works on file4d4415e05f23.zip", {
  skip_if_not_installed("sf")

  # Path to the ZIP file
  zip_path <- system.file("extdata", "file4d4415e05f23.zip", package = "frheritage")
  expect_true(file.exists(zip_path))

  # Read shapefiles
  sf_list <- geo_shapefiles_read(zip_path, crs = 2154)

  # Basic checks
  expect_type(sf_list, "list")
  expect_true(length(sf_list) > 0)  # Ensure at least one shapefile was read

  # All elements are sf objects
  expect_true(all(vapply(sf_list, inherits, logical(1), what = "sf")))

  # All sf objects have at least one row
  expect_false(all(vapply(sf_list, function(x) nrow(x) > 0, logical(1))))

  # All sf objects are in EPSG:2151
  expect_true(all(vapply(sf_list, function(x) sf::st_crs(x)$epsg == 2154, logical(1))))
})
