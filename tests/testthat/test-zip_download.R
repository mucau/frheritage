test_that("zip_download() returns NULL on simulated HTTP error", {
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_perform = function(req) structure(list(
      body = raw(0), status_code = 404
    ), class = "response_httr2"),
    resp_status = function(resp) resp$status_code,
    resp_body_raw = function(resp) resp$body,
    .package = "httr2"
  )

  tmpf <- suppressWarnings(zip_download("http://fake-url/file.zip", id = "4d441aa75cf1"))
  expect_null(tmpf)
})

test_that("zip_download() handles network errors gracefully", {

  url <- "http://fake-url/file.zip"
  id <- "123"

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_perform = function(req) stop("Network error"),
    .package = "httr2"
  )

  expect_warning(
    tmpf <- zip_download(url, id),
    regexp = "Download error for ID 123",
    fixed = FALSE
  )
  expect_null(tmpf)
})
