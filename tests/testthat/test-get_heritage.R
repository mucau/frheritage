test_that("get_heritage() retrieves one heritage data correctly", {
  skip_if_offline()
  skip_if_not_installed("httptest2")
  skip_if_not_installed("sf")

  sevres_path <- system.file("extdata/sevres.Rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  httptest2::with_mock_dir("mock_get_heritage_single", {
    result <- get_heritage(
      x = sevres,
      data_code = "IMMH",
      buffer = 500,
      crs = 2154,
      verbose = FALSE
    )

    expect_s3_class(result, "sf")
    expect_true(nrow(result) > 0)
    expect_true("geometry" %in% names(result))
    expect_identical(sf::st_crs(result)$epsg, as.integer(2154))
  })
})

test_that("get_heritage() retrieves several heritage data correctly", {
  skip_if_offline()
  skip_if_not_installed("httptest2")
  skip_if_not_installed("sf")

  sevres_path <- system.file("extdata/sevres.Rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  httptest2::with_mock_dir("mock_get_heritage_multiple", {
    result <- get_heritage(
      x = sevres,
      data_code = get_heritage_layernames()$code,
      buffer = 500,
      crs = 2154,
      verbose = FALSE
    )

    expect_true(is.list(result))
    expect_true(all(vapply(result, inherits, logical(1), what = "sf")))
    expect_true(all(vapply(result, function(x) nrow(x) > 0, logical(1))))
  })
})
