test_that("geo_spatial_check() validates spatial predicates correctly", {

  valid_predicates <- c(
    "INTERSECTS", "DISJOINT", "CONTAINS", "WITHIN",
    "TOUCHES", "BBOX", "CROSSES", "OVERLAPS",
    "EQUALS", "RELATE", "DWITHIN", "BEYOND"
  )

  # Test: accepts valid predicates (case-insensitive)
  for (predicate in valid_predicates) {
    expect_identical(geo_spatial_check(predicate), predicate)
    expect_identical(geo_spatial_check(tolower(predicate)), predicate)
    expect_identical(geo_spatial_check(tools::toTitleCase(tolower(predicate))), predicate)
  }

  # Test: only the first element of a vector is used
  expect_identical(
    geo_spatial_check(c("intersects", "within")),
    "INTERSECTS"
  )

  # Test: raises error for invalid values
  expect_error(
    geo_spatial_check("INVALID"),
    regexp = "`spatial_filter` must be one of"
  )

  # Test: raises error for NA or empty input
  expect_error(geo_spatial_check(NA_character_), regexp = "`spatial_filter` must be one of")
  expect_error(geo_spatial_check(""), regexp = "`spatial_filter` must be one of")

  # Test: output is always a single uppercase string
  result <- geo_spatial_check("within")
  expect_type(result, "character")
  expect_equal(length(result), 1L)
  expect_true(result == toupper(result))
})
