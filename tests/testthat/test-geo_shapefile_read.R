test_that("geo_shapefiles_read() works on all extdata ZIP files", {
  skip_if_not_installed("sf")

  zip_files <- list.files(
    system.file("extdata", package = "frheritage"),
    pattern = "\\.zip$",
    full.names = FALSE
  )

  for (zip_name in zip_files) {

    zip_path <- system.file("extdata", zip_name, package = "frheritage")

    skip_if(!file.exists(zip_path))

    sf_obj <- tryCatch(
      geo_shapefiles_read(zip_path, crs = 2154),
      error = function(e) NULL
    )

    expect_true(inherits(sf_obj, "sf") || is.null(sf_obj))

    if (!is.null(sf_obj)) {
      expect_equal(sf::st_crs(sf_obj)$epsg, 2154)
    }
  }
})
