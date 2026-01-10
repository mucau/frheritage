# Functional test: one valid code, returns a single sf object
test_that("get_heritage() returns sf object with valid mocked data", {
  # Input: simple sf point
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  # Mock dependencies
  local_mocked_bindings(
    data_check = function(code) TRUE,
    geo_spatial_check = function(filter) filter,
    geo_too_large = function(x, verbose) NULL,
    geo_prepare = function(x, crs, buffer) x,
    geo_extent = function(y) c(2.1, 48.7, 2.3, 48.9),
    geo_dep = function(x) c("92"),
    data_filter = function(department, data_code) {
      data.frame(
        id = c("1", "2"),
        title = c("Title 1", "Title 2"),
        guid = c("G1", "G2"),
        code = data_code,
        stringsAsFactors = FALSE
      )
    },
    zip_query_build = function(id, title, guid, extent_vals, crs) "http://fake-url.zip",
    zip_download = function(url, id, verbose) system.file("extdata/file4d441aa75cf1.zip", package = "frheritage"),
    geo_shapefiles_read = function(zip_tmp, crs) sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = crs)),
    geo_sf_bind = function(lst) do.call(rbind, lst),
    .package = "frheritage"
  )

  res <- get_heritage(x, data_code = "IMMH", verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2)  # two rows because data_filter mocked two IDs
})

# Non-functional test: invalid extent triggers stop
test_that("get_heritage() stops if extent is invalid", {
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    data_check = function(code) TRUE,
    geo_spatial_check = function(filter) filter,
    geo_too_large = function(x, verbose) NULL,
    geo_prepare = function(x, crs, buffer) x,
    geo_extent = function(y) NULL,
    geo_dep = function(x) c("92"),
    .package = "frheritage"
  )

  expect_error(get_heritage(x, data_code = "IMMH"), "Invalid extent")
})

# Non-functional test: no department triggers stop
test_that("get_heritage() stops if no department found", {
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    data_check = function(code) TRUE,
    geo_spatial_check = function(filter) filter,
    geo_too_large = function(x, verbose) NULL,
    geo_prepare = function(x, crs, buffer) x,
    geo_extent = function(y) c(2.1, 48.7, 2.3, 48.9),
    geo_dep = function(x) NULL,
    .package = "frheritage"
  )

  expect_error(get_heritage(x, data_code = "IMMH"), "Invalid dep")
})

# Non-functional test: zip_download returns NULL
test_that("get_heritage() stops if download fails", {
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    data_check = function(code) TRUE,
    geo_spatial_check = function(filter) filter,
    geo_too_large = function(x, verbose) NULL,
    geo_prepare = function(x, crs, buffer) x,
    geo_extent = function(y) c(2.1, 48.7, 2.3, 48.9),
    geo_dep = function(x) c("92"),
    data_filter = function(department, data_code) {
      data.frame(
        id = "1",
        title = "Title 1",
        guid = "G1",
        code = data_code,
        stringsAsFactors = FALSE
      )
    },
    zip_query_build = function(...) "http://fake-url.zip",
    zip_download = function(...) NULL,   # simulate download failure
    geo_sf_bind = function(lst) NULL,
    .package = "frheritage"
  )

  expect_error(get_heritage(x, data_code = "IMMH", verbose = FALSE),
               "No spatial data could be retrieved")
})
