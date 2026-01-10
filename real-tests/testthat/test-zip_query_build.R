test_that("zip_query_build() constructs a valid query URL", {
  skip_if_not_installed("sf")

  # Load internal sf object
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Compute extent
  ext <- geo_extent(sevres)
  expect_length(ext, 4)

  # Build URL
  url <- zip_query_build(
    id = 123,
    title = "Example Dataset",
    extent_vals = ext,
    crs = 2154
  )

  # Check URL format
  expect_type(url, "character")
  expect_match(url, "^http://atlas\\.patrimoines\\.culture\\.fr/services/export\\.php")
  expect_match(url, "data=")

  # URL should contain encoded JSON content
  expect_match(url, "SHP")
  expect_match(url, "EPSG")
  expect_match(url, "2154")
})
