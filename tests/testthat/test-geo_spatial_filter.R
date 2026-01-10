test_that("geo_spatial_filter() correctly applies spatial predicates", {
  skip_if_not_installed("sf")

  # Prepare sample geometries: 3 polygons and points
  polys <- sf::st_as_sf(
    data.frame(
      id = 1:3,
      geom = sf::st_sfc(
        sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
        sf::st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0)))),
        sf::st_polygon(list(rbind(c(0,2), c(1,2), c(1,3), c(0,3), c(0,2))))
      )
    )
  )

  point_inside  <- sf::st_sfc(sf::st_point(c(0.5, 0.5)))
  point_outside <- sf::st_sfc(sf::st_point(c(5, 5)))
  point_border  <- sf::st_sfc(sf::st_point(c(1, 0.5)))

  x_inside  <- sf::st_as_sf(data.frame(id = 1, geom = point_inside))
  x_outside <- sf::st_as_sf(data.frame(id = 2, geom = point_outside))
  x_border  <- sf::st_as_sf(data.frame(id = 3, geom = point_border))

  # Test: INTERSECTS finds overlapping features
  res_intersects <- quiet(geo_spatial_filter(polys, x_inside, "INTERSECTS"))
  expect_s3_class(res_intersects, "sf")
  expect_equal(nrow(res_intersects), 1)
  expect_identical(res_intersects$id, 1L)

  # Test: DISJOINT finds non-intersecting features
  res_disjoint <- quiet(geo_spatial_filter(polys, x_outside, "DISJOINT"))
  expect_s3_class(res_disjoint, "sf")
  expect_equal(nrow(res_disjoint), 3)

  # Test: WITHIN behaves correctly
  res_within <- quiet(geo_spatial_filter(polys, x_inside, "WITHIN"))
  expect_s3_class(res_within, "sf")

  # Test: TOUCHES detects border contact
  res_touches <- quiet(geo_spatial_filter(polys, x_border, "TOUCHES"))
  expect_s3_class(res_touches, "sf")

  # Test: unsupported predicate raises an error
  expect_error(
    geo_spatial_filter(polys, x_inside, "INVALID"),
    regexp = "Invalid `spatial_filter`"
  )

  # Test: returned object keeps sf structure and CRS
  expect_s3_class(res_intersects, "sf")
  expect_identical(sf::st_crs(res_intersects), sf::st_crs(polys))
})
