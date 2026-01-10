test_that("ids_download() works with single mocked item", {
  url <- "http://fake-url"

  local_mocked_bindings(
    req_perform = function(req) {
      structure(list(
        status_code = 200,
        body = '<?xml version="1.0"?><rss><channel><item>
                  <title>Test record</title>
                  <guid>MD_1234</guid>
                </item></channel></rss>'
      ), class = "response_httr2_mock")
    },
    resp_status = function(res) 200,
    resp_body_string = function(res) res$body,
    .package = "httr2"
  )

  df <- ids_download(url)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_equal(df$id, "1234")
  expect_equal(df$title, "Test record")
  expect_equal(df$guid, "MD_1234")
})

test_that("ids_download() works with multiple mocked items", {
  url <- "http://fake-url"

  local_mocked_bindings(
    req_perform = function(req) {
      structure(list(
        status_code = 200,
        body = '<?xml version="1.0"?><rss><channel>
                  <item><title>Record 1</title><guid>MD_111</guid></item>
                  <item><title>Record 2</title><guid>MD_222</guid></item>
                  <item><title>Record 3</title><guid>MD_333</guid></item>
                </channel></rss>'
      ), class = "response_httr2_mock")
    },
    resp_status = function(res) 200,
    resp_body_string = function(res) res$body,
    .package = "httr2"
  )

  df <- ids_download(url)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(df$id, c("111", "222", "333"))
})

test_that("ids_download() returns empty dataframe when XML has no items", {
  url <- "http://fake-url"

  local_mocked_bindings(
    req_perform = function(req) {
      structure(list(
        status_code = 200,
        body = '<?xml version="1.0"?><rss><channel></channel></rss>'
      ), class = "response_httr2_mock")
    },
    resp_status = function(res) 200,
    resp_body_string = function(res) res$body,
    .package = "httr2"
  )

  df <- ids_download(url)|> suppressWarnings()

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

test_that("ids_download() handles network errors gracefully", {
  url <- "http://fake-url"

  local_mocked_bindings(
    req_perform = function(req) stop("Network error"),
    .package = "httr2"
  )

  expect_error(suppressWarnings(ids_download(url)))
})
