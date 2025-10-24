test_that("geo_object_type() correctly identifies geometry types", {
  point <- sf::st_sfc(sf::st_point(c(1, 1)))
  multipoint <- sf::st_sfc(sf::st_multipoint(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE)))
  line <- sf::st_sfc(sf::st_linestring(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE)))
  multiline <- sf::st_sfc(sf::st_multilinestring(list(matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE))))
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE))))
  multipolygon <- sf::st_sfc(sf::st_multipolygon(list(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE)))))

  expect_identical(geo_object_type(point), "POINT")
  expect_identical(geo_object_type(multipoint), "POINT")       # mapped to "POINT"
  expect_identical(geo_object_type(line), "LINE")              # mapped to "LINE"
  expect_identical(geo_object_type(multiline), "LINE")         # mapped to "LINE"
  expect_identical(geo_object_type(polygon), "POLYGON")
  expect_identical(geo_object_type(multipolygon), "POLYGON")   # mapped to "POLYGON"
})
