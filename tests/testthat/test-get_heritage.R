test_that("get_heritage() returns sf object with valid mocked data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,

    get_deps = function(x) c("92"),

    zip_query_build = function(...) "http://fake-url.zip",
    zip_download = function(...) system.file("extdata/file4d441aa75cf1.zip", package = "frheritage"),

    geo_shapefile_read = function(zip_tmp, crs) {
      sf::st_sf(
        id = "1",
        geometry = sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = crs)
      )
    },

    geo_sf_bind = function(lst) do.call(rbind, lst),

    .package = "frheritage"
  )

  res <- get_heritage(x, data_code = "IMMH", verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
})

test_that("get_heritage() stops if extent is invalid", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    geo_extent = function(y) NULL,
    get_deps = function(x) c("92"),
    .package = "frheritage"
  )

  expect_error(
    get_heritage(x, data_code = "IMMH"),
    "Invalid extents"
  )
})

test_that("get_heritage() stops if no department found", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(x) NULL,
    .package = "frheritage"
  )

  expect_error(
    get_heritage(x, data_code = "IMMH"),
    "At least one geometry has no department"
  )
})

test_that("get_heritage() returns empty sf and warning if download fails", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(x) c("92"),
    zip_query_build = function(...) "http://fake-url.zip",
    zip_download = function(...) NULL,
    .package = "frheritage"
  )

  res <- get_heritage(x, data_code = "IMMH", verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("get_heritage() fails with multiple data_code", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    .package = "frheritage"
  )

  expect_error(
    get_heritage(x, data_code = c("A", "B")),
    "`data_code` must be one of:"
  )
})

test_that("get_heritage() returns empty sf if no IDs found", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) "92",
    data_filter = function(...) data.frame(),
    .package = "frheritage"
  )

  res <- get_heritage(x, data_code = "IMMH", verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("get_heritage() handles no matching code rows", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) "92",
    data_filter = function(...) {
      data.frame(
        id = "1",
        title = "t",
        guid = "g",
        departement = "OTHER"
      )
    },
    .package = "frheritage"
  )

  res <- get_heritage(x, data_code = "IMMH", verbose = FALSE)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("get_heritage() returns empty sf if all downloads fail", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) "92",
    zip_download = function(...) NULL,
    geo_sf_bind = function(...) NULL,
    .package = "frheritage"
  )

  res <- get_heritage(x, data_code = "IMMH", verbose = FALSE)

  expect_equal(nrow(res), 0)
})

test_that("get_heritage() warns on partial download failure", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.2, 48.8)), crs = 4326))

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) "92",
    zip_download = function(url, id) if (id == "1") "ok" else NULL,
    geo_shapefile_read = function(...) sf::st_as_sf(sf::st_sfc(sf::st_point(c(0,0)), crs = 2154)),
    geo_sf_bind = function(lst) do.call(rbind, lst),
    .package = "frheritage"
  )

  expect_message(
    res <- get_heritage(x, data_code = "IMMH", verbose = TRUE),
    "could not be retrieved|Download failed|Empty/invalid"
  ) |> quiet()

  expect_s3_class(res, "sf")
})
