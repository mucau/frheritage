# mock tests ----
test_that("atlas_ok returns TRUE on HTTP 200", {

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      httr2::response(
        status_code = 200,
        headers = list(),
        body = raw()
      )
    },
    .package = "httr2"
  )

  expect_true(atlas_ok("http://fake.url"))
})

test_that("atlas_ok returns TRUE on HTTP 302", {

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      httr2::response(
        status_code = 302,
        headers = list(),
        body = raw()
      )
    },
    .package = "httr2"
  )

  expect_true(atlas_ok("http://fake.url"))
})

test_that("atlas_ok returns FALSE on HTTP 500", {

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      httr2::response(
        status_code = 500,
        headers = list(),
        body = raw()
      )
    },
    .package = "httr2"
  )

  expect_false(atlas_ok("http://fake.url"))
})

test_that("atlas_ok returns FALSE on timeout", {

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      stop(structure(list(), class = c("httr2_timeout", "error")))
    },
    .package = "httr2"
  )

  expect_false(atlas_ok("http://fake.url"))
})

test_that("atlas_ok returns FALSE on generic error", {

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      stop("connection failed")
    },
    .package = "httr2"
  )

  expect_false(atlas_ok("http://fake.url"))
})

test_that("atlas_ok returns a single logical", {

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      httr2::response(200, list(), raw())
    },
    .package = "httr2"
  )

  res <- atlas_ok("http://fake.url")

  expect_type(res, "logical")
  expect_length(res, 1)
})

# real tests ----
test_that("atlas_ok real network behavior", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # OK
  expect_true(
    atlas_ok("https://www.google.com", timeout = 3)
  )

  # DNS invalide
  expect_false(
    atlas_ok("http://example.invalid", timeout = 2)
  )

  # Timeout
  expect_false(
    atlas_ok("http://10.255.255.1", timeout = 2)
  )

  # HTTP error
  expect_false(
    atlas_ok("https://httpbin.org/status/500", timeout = 3)
  )
})

