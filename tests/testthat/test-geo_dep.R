test_that("geo_dep() retrieves the correct INSEE department code for sevres", {
  skip_if_offline()
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  # Load example sf object
  sevres_path <- system.file("extdata", "sevres.Rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")

  # Run geo_dep()
  deps <- geo_dep(sevres)

  # Check result
  expect_true(is.character(deps))
  expect_true("92" %in% deps)
  expect_length(deps, 1)
})
