test_that("zip_query_build() builds a valid URL with minimal inputs", {
  id <- 1234
  title <- "Test Title"
  extent_vals <- c(100, 200, 300, 400)

  url <- zip_query_build(id, title, extent_vals = extent_vals, crs = 2154)

  expect_type(url, "character")
  expect_true(grepl("^http://atlas.patrimoines.culture.fr/services/export.php\\?data=", url))
  expect_true(grepl("EPSG%3A2154", url))
  expect_true(grepl("Test Title", URLdecode(url)))
  expect_true(grepl("123", URLdecode(url)))
})

test_that("zip_query_build() includes guid when provided", {
  id <- 4567
  title <- "Another Test"
  guid <- "MD_456"
  extent_vals <- c(10, 20, 30, 40)

  url <- zip_query_build(id, title, guid = guid, extent_vals = extent_vals)

  decoded <- URLdecode(url)
  expect_true(grepl("MD_456", decoded))
  expect_true(grepl("Another Test", decoded))
})

test_that("zip_query_build() fails with invalid extent length", {
  id <- 1
  title <- "Fail Case"
  extent_vals <- c(1,2,3)  # only 3 values

  expect_error(
    zip_query_build(id, title, extent_vals = extent_vals),
    "extent_vals must have 4 numeric values"
  )
})

test_that("zip_query_build() works with different CRS", {
  id <- 6789
  title <- "CRS Test"
  extent_vals <- c(0, 0, 1, 1)

  # Using a common geographic CRS (EPSG:4326)
  url <- zip_query_build(id, title, extent_vals = extent_vals, crs = 4326)

  decoded <- URLdecode(url)
  expect_true(grepl("EPSG:4326", decoded))
})

test_that("zip_query_build() JSON format uses single quotes after gsub", {
  id <- 1001
  title <- "Quotes Test"
  extent_vals <- c(0, 0, 1, 1)

  url <- zip_query_build(id, title, extent_vals = extent_vals)
  expect_false(grepl('\\"', url))  # No double quotes
  expect_true(grepl("SHP", URLdecode(url)))  # format still included
})
