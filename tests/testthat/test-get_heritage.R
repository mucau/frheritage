test_that("get_heritage() fails when data_code has length > 1", {
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)

  expect_error(
    get_heritage(sevres, data_code = c("IMMH", "SICI")),
    "`data_code` must be a single heritage code"
  )
})

test_that("get_heritage() errors when data_code is invalid", {
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)

  expect_error(
    get_heritage(sevres, data_code = "INVALID_CODE"),
    "must be one of"
  )
})

test_that("get_heritage() retrieves one heritage data correctly", {
  skip_if_not_installed("sf")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")


  result <- get_heritage(x = sevres,
                         data_code = "IMMH",
                         buffer = 500,
                         crs = 2154,
                         verbose = FALSE)


  expect_s3_class(result, "sf")
  expect_true(nrow(result) > 0)
  expect_true("geometry" %in% names(result))
  expect_identical(sf::st_crs(result)$epsg, as.integer(2154))
})
