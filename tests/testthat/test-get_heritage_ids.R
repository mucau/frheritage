test_that("get_heritage_ids() returns combined heritage IDs", {

  x <- sf::st_as_sf(sf::st_sfc(
    sf::st_point(c(2.209271, 48.82095)), crs = 4326
  ))

  mocked_get_deps <- function(...) c("92")

  mocked_geo_extent <- function(...) {
    data.frame(
      left = 2.17522401805063,
      bottom = 48.7984671628133,
      right = 2.24331827597942,
      top = 48.8434323867205
    )
  }

  mocked_ids_url_build <- function(extent_row, dep) "http://fake-url"

  mocked_ids_download <- function(url, ...) {
    data.frame(
      id = c("4358", "3872", "3464"),
      title = c(
        "Registered Site - Ile-de-France (AC2)",
        "UNESCO World Heritage - Buffer zone footprint",
        "Classified buildings - Hauts-de-Seine - 92"
      ),
      guid = c("4358", "3872", "3464"),
      stringsAsFactors = FALSE
    )
  }

  mocked_ids_to_codes <- function(titles) {
    paste0("code_", seq_along(titles))
  }

  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = mocked_get_deps,
    geo_extent = mocked_geo_extent,
    ids_url_build = mocked_ids_url_build,
    ids_download = mocked_ids_download,
    ids_to_codes = mocked_ids_to_codes,
    .package = "frheritage"
  )

  res <- get_heritage_ids(x, verbose = FALSE)

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

  # Case 1: no department returned
  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) list(),  # important: empty
    geo_extent = function(...) data.frame(
      left = 1, bottom = 1, right = 2, top = 2
    ),
    .package = "frheritage"
  )

  expect_error(
    get_heritage_ids(x, verbose = FALSE),
    "At least one geometry has no department"
  )

  # Case 2: geo_extent returns NULL
  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) c("92"),
    geo_extent = function(...) NULL,
    .package = "frheritage"
  )

  expect_error(
    get_heritage_ids(x, verbose = FALSE),
    "Invalid extents\\."
  )

  # Case 3: empty download result
  local_mocked_bindings(
    atlas_ok = function() TRUE,
    get_deps = function(...) c("92"),
    geo_extent = function(...) data.frame(
      left = 1, bottom = 1, right = 2, top = 2
    ),
    ids_url_build = function(...) "http://fake-url",
    ids_download = function(...) data.frame(),
    ids_to_codes = function(...) character(0),
    .package = "frheritage"
  )

  expect_warning(
    res <- get_heritage_ids(x, verbose = FALSE),
    "No heritage identifiers were retrieved"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})
