test_that("ids_url_build() builds a valid proxy URL", {
  extent <- list(left = 1, bottom = 2, right = 3, top = 4)
  url <- ids_url_build(extent, "75")

  expect_type(url, "character")
  expect_match(url, "^http://atlas\\.patrimoines\\.culture\\.fr/atlas/trunk/proxy_ign\\.php\\?url=")

  # Decode URL for readable matching
  decoded <- utils::URLdecode(url)
  expect_match(decoded, "geosource/srv/fr/rss\\.search")
  expect_match(decoded, "DEP_75")
  expect_match(decoded, "geoForm=BBOX")
})


test_that("ids_url_build() encodes special characters safely", {
  extent <- list(left = 0, bottom = 0, right = 1, top = 1)
  url <- ids_url_build(extent, "2A") # Corsican department

  expect_true(!grepl(" ", url))  # No spaces
  expect_true(grepl("DEP_2A", url))  # Properly encoded department code
})

test_that("ids_url_build() handles numeric department codes", {
  extent <- list(left = 0, bottom = 0, right = 1, top = 1)
  url <- ids_url_build(extent, 33)

  decoded <- utils::URLdecode(url)
  expect_match(decoded, "geoID=DEP_33")
})

test_that("ids_url_build() fails gracefully on malformed extent", {
  bad_extent <- list(left = 0, bottom = 0)  # Missing right/top
  expect_error(
    ids_url_build(bad_extent, "75"),
    regexp = "missing.*right|top",
    fixed = FALSE
  )
})

