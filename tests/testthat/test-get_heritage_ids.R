test_that("get_heritage_ids() returns combined heritage IDs", {

  # Input sf point
  x <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(2.209271, 48.82095)), crs = 4326
  ))

  # Mock for geo_dep()
  mocked_geo_dep <- function(...) c("92")  # single department

  # Mock for geo_extent()
  mocked_geo_extent <- function(...) {
    matrix(c(
      2.17522401805063, 2.24331827597942,
      48.7984671628133, 48.8434323867205
    ), nrow = 1, byrow = TRUE,
    dimnames = list(NULL, c("west", "east", "south", "north")))
  }

  # Mock for ids_url_build()
  mocked_ids_url_build <- function(extent_row, dep) {
    "http://fake-url"
  }

  # Mock for ids_download()
  mocked_ids_download <- function(url, ...) {
    data.frame(
      id = c("4358", "3872", "3464"),
      title = c(
        "Registered Site - Ile-de-France (AC2)",
        "UNESCO World Heritage - Buffer zone footprint of the properties - Ile-de-France",
        "Classified or registered buildings - Hauts-de-Seine - 92"
      ),
      guid = c("4358", "3872", "3464"),
      stringsAsFactors = FALSE
    )
  }

  # Mock for ids_to_codes()
  mocked_ids_to_codes <- function(titles) {
    paste0("code_", seq_along(titles))
  }

  # Apply mocks
  testthat::local_mocked_bindings(
    geo_dep = mocked_geo_dep,
    geo_extent = mocked_geo_extent,
    ids_url_build = mocked_ids_url_build,
    ids_download = mocked_ids_download,
    ids_to_codes = mocked_ids_to_codes,
    .package = "frheritage"
  )

  # Call the function
  res <- get_heritage_ids(x, verbose = FALSE)

  # Checks
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$id, c("4358", "3872", "3464"))
  expect_equal(res$guid, c("4358", "3872", "3464"))
  expect_equal(res$code, c("code_1", "code_2", "code_3"))
})

test_that("get_heritage_ids() handles errors gracefully", {

  x <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(2.209271, 48.82095)), crs = 4326
  ))

  # Case 1: geo_dep() returns NULL → stop("Invalid dep.")
  local_mocked_bindings(
    geo_dep = function(...) NULL,
    geo_extent = function(...) matrix(1:4, nrow = 1),
    .package = "frheritage"
  )
  expect_error(get_heritage_ids(x, verbose = FALSE), "Invalid dep\\.")

  # Case 2: geo_extent() returns NULL → stop("Invalid extent.")
  local_mocked_bindings(
    geo_dep = function(...) c("92"),
    geo_extent = function(...) NULL,
    .package = "frheritage"
  )
  expect_error(get_heritage_ids(x, verbose = FALSE), "Invalid extent\\.")

  # Case 3: ids_download() returns empty df → warning "No heritage identifiers were retrieved."
  local_mocked_bindings(
    geo_dep = function(...) c("92"),
    geo_extent = function(...) matrix(1:4, nrow = 1),
    ids_url_build = function(extent_row, dep) "http://fake-url",
    ids_download = function(url, ...) data.frame(),
    ids_to_codes = function(titles) character(0),
    .package = "frheritage"
  )
  expect_warning(res <- get_heritage_ids(x, verbose = FALSE), "No heritage identifiers were retrieved\\.")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})
