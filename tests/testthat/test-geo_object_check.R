test_that("geo_object_check() validates sf objects correctly", {
  skip_if_not_installed("sf")

  # Create valid geometries
  point <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 1))))
  multipoint <- sf::st_sf(geometry = sf::st_sfc(sf::st_multipoint(matrix(c(1,1,2,2), ncol = 2, byrow = TRUE))))
  line <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(matrix(c(1,1,2,2), ncol = 2, byrow = TRUE))))
  multiline <- sf::st_sf(geometry = sf::st_sfc(sf::st_multilinestring(list(matrix(c(1,1,2,2), ncol = 2, byrow = TRUE)))))
  polygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE)))))
  multipolygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_multipolygon(list(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE))))))

  # Valid cases: should not raise errors
  expect_silent(geo_object_check(point, allowed_geom_classes = "POINT"))
  expect_silent(geo_object_check(multipoint, allowed_geom_classes = "POINT"))
  expect_silent(geo_object_check(line, allowed_geom_classes = "LINE"))
  expect_silent(geo_object_check(multiline, allowed_geom_classes = "LINE"))
  expect_silent(geo_object_check(polygon, allowed_geom_classes = "POLYGON"))
  expect_silent(geo_object_check(multipolygon, allowed_geom_classes = "POLYGON"))

  # Invalid class: not an sf object
  not_sf <- data.frame(x = 1:2, y = 3:4)
  expect_error(geo_object_check(not_sf), "must be of class 'sf'")

  # Invalid geometry type
  expect_error(
    geo_object_check(point, allowed_geom_classes = "POLYGON"),
    regexp = "must have geometry type"
  )

  # Invalid geometries
  invalid_poly <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,1, 1,0, 0,1, 0,0), ncol = 2, byrow = TRUE)))
    )
  )
  expect_error(
    geo_object_check(invalid_poly),
    "contains invalid geometries"
  )

  # Empty geometries
  empty_geom <- sf::st_sf(geometry = sf::st_sfc(sf::st_geometrycollection()))
  expect_error(geo_object_check(empty_geom), "contains empty geometries")
})
