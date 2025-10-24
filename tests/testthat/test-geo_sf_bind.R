test_that("geo_sf_bind() combines sf objects correctly", {
  skip_if_not_installed("sf")

  # Prepare minimal test geometries
  sf1 <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(
    sf::st_point(c(0,0)), sf::st_point(c(1,1))
  ))
  sf2 <- sf::st_sf(a = 3:4, geometry = sf::st_sfc(
    sf::st_point(c(2,2)), sf::st_point(c(3,3))
  ))
  sf3 <- sf::st_sf(b = 5:6, geometry = sf::st_sfc(
    sf::st_point(c(4,4)), sf::st_point(c(5,5))
  ))
  empty_sf <- sf::st_sf(a = integer(0), geometry = sf::st_sfc())

  # Test: simple bind
  res <- geo_sf_bind(list(sf1, sf2))
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 4)
  expect_true(all(c("a", "geometry") %in% colnames(res)))

  # Test: bind with differing columns
  res2 <- geo_sf_bind(list(sf1, sf3))
  expect_s3_class(res2, "sf")
  expect_equal(nrow(res2), 4)
  expect_true(all(c("a", "b", "geometry") %in% colnames(res2)))
  expect_true(all(is.na(res2$b[1:2])))  # missing values filled with NA
  expect_true(all(is.na(res2$a[3:4])))

  # Test: nested list of sf
  nested <- list(list(sf1, sf2))
  res3 <- geo_sf_bind(nested)
  expect_s3_class(res3, "sf")
  expect_equal(nrow(res3), 4)

  # Test: list with NULL or empty sf
  res4 <- geo_sf_bind(list(sf1, NULL, empty_sf, sf2))
  expect_s3_class(res4, "sf")
  expect_equal(nrow(res4), 4)

  # Test: empty input returns NULL
  expect_null(geo_sf_bind(list()))
})
