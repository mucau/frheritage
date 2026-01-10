test_that("zip_download() can fetch a real ZIP from Atlas du Patrimoine", {
  skip_if_not_installed("httr2")
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  # Real query URL
  url <- "http://atlas.patrimoines.culture.fr/services/export.php?data=%7B%22extent%22%3A%7B%22left%22%3A2.1786%2C%22bottom%22%3A48.8092%2C%22right%22%3A2.2327%2C%22top%22%3A48.8354%7D%2C%22format%22%3A%22SHP%22%2C%22srs%22%3A%22EPSG%3A2154%22%2C%22md%22%3A%5B%7B%22id%22%3A%22123%22%2C%22title%22%3A%22Example%20Dataset%22%7D%5D%7D"

  # Perform real download
  tmp <- zip_download(url, id = 123)

  # Check result
  expect_type(tmp, "character")
  expect_match(tmp, "\\.zip$")
  expect_true(file.exists(tmp))

  # Check file content (should be valid ZIP header)
  con <- file(tmp, "rb")
  header <- readBin(con, "raw", 4)
  close(con)
  expect_identical(header, charToRaw("PK\x03\x04"))

  # Cleanup
  unlink(tmp)
})
