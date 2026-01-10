test_that("ids_download() retrieves and parses data correctly", {
  skip_if_not_installed("sf")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  # Load internal sf object
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  skip_if_not(file.exists(sevres_path), "Missing test data: sevres.rda")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Build the URL
  url <- ids_url_build(geo_extent(sevres), 92)
  expect_match(url, "^http://atlas\\.patrimoines\\.culture\\.fr")

  # Run
  result <- ids_download(url)

  # Check the structure of the result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "title") %in% names(result)))

  # Optional: check that result has at least one row
  expect_gt(nrow(result), 0)
})
