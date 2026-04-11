test_that("get_deps() works", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2.209271, 48.82095)), crs = 4326))
  res <- sf::st_sf(
    code_insee = "92",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(2.18, 48.80),
        c(2.24, 48.80),
        c(2.24, 48.85),
        c(2.18, 48.85),
        c(2.18, 48.80)
      ))),
      crs = 4326
    )
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) res,
    .package = "happign"
  )

  dep <- get_deps(x)

  # Checks
  expect_true(is.character(dep))
  expect_true("92" %in% dep)
  expect_length(dep, 1)
})

test_that("get_deps() returns null when no data", {
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  res <- sf::st_sf(code_insee = character(0),
                   geometry = sf::st_sfc(crs = 4326))

  testthat::local_mocked_bindings(
    get_wfs = function(...) res,
    .package = "happign"
  )

  dep <- quiet(get_deps(x))

  # tests
  expect_null(dep)
})

test_that("get_deps() handles WFS error", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(2, 48)), crs = 4326))

  testthat::local_mocked_bindings(
    get_wfs = function(...) stop("WFS error"),
    .package = "happign"
  )

  expect_message(
    res <- get_deps(x),
    "get_wfs\\(\\) failed"
  ) |> quiet()

  expect_null(res)
})
