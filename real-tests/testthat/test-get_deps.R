test_that("get_deps() retrieves the correct INSEE department code for sevres", {

  skip_if_not_installed("sf")
  skip_if_not_installed("happign")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  # Load example sf object
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  skip_if_not(file.exists(sevres_path), "Missing test data: sevres.rda")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Use mock responses for HTTP requests
  deps <- quiet(get_deps(sevres))

  # Checks
  expect_true(is.character(deps))
  expect_true("92" %in% deps)
  expect_length(deps, 1)
})

test_that("get_deps() works on real WFS (single point)", {

  skip_if_not_installed("sf")
  skip_if_not_installed("happign")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_point(c(2.35, 48.85)),
      crs = 4326
    )
  )

  deps <- get_deps(x)

  # Checks (souples)
  expect_true(is.character(deps))
  expect_true(length(deps) >= 1)
  expect_true(any(deps %in% c("75")))  # Paris
})

test_that("get_deps() works on real WFS (multiple points)", {

  skip_if_not_installed("sf")
  skip_if_not_installed("happign")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_point(c(2.35, 48.85)),  # Paris
      sf::st_point(c(1.09, 49.44)),  # Rouen
      crs = 4326
    )
  )

  deps <- get_deps(x)

  # Checks
  expect_true(is.character(deps))
  expect_true(length(deps) >= 2)
  expect_true(all(c("75", "76") %in% deps))
})
