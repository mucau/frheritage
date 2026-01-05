# Prepare sample geometries
point <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0,0))))
multipoint <- sf::st_sf(geometry = sf::st_sfc(sf::st_multipoint(matrix(c(0,0,1,1), ncol = 2, byrow = TRUE))))
line <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(matrix(c(0,0,1,1), ncol = 2, byrow = TRUE))))
multiline <- sf::st_sf(geometry = sf::st_sfc(sf::st_multilinestring(list(matrix(c(0,0,1,1), ncol = 2, byrow = TRUE)))))
polygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE)))))
multipolygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_multipolygon(list(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE))))))

test_that("geo_cast() converts multi-part geometries to simple types", {
  expect_identical(as.character(sf::st_geometry_type(geo_cast(multipoint))), c("POINT", "POINT"))
  expect_identical(as.character(sf::st_geometry_type(geo_cast(multiline))), "LINESTRING")
  expect_identical(as.character(sf::st_geometry_type(geo_cast(multipolygon))), "POLYGON")
})

test_that("geo_cast() leaves single-part geometries unchanged", {
  expect_identical(as.character(sf::st_geometry_type(geo_cast(point))), "POINT")
  expect_identical(as.character(sf::st_geometry_type(geo_cast(line))), "LINESTRING")
  expect_identical(as.character(sf::st_geometry_type(geo_cast(polygon))), "POLYGON")
})

test_that("geo_cast() handles mixed geometry types feature by feature", {
  mixed_points <- rbind(point, multipoint)
  res <- geo_cast(mixed_points)
  geom_types <- as.character(st_geometry_type(res))
  expect_true(all(geom_types %in% "POINT"))

  mixed_polys <- rbind(polygon, multipolygon)
  res2 <- geo_cast(mixed_polys)
  geom_types2 <- as.character(st_geometry_type(res2))
  expect_true(all(geom_types2 %in% "POLYGON"))
})

test_that("geo_cast() runs silently on multiple geometries", {
  mixed_points <- rbind(point, multipoint)
  res <- geo_cast(mixed_points)
  geom_types <- as.character(sf::st_geometry_type(res))
  expect_true(all(geom_types %in% "POINT"))
})

