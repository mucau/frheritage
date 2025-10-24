test_that("data_check() passes with valid codes", {
  expect_silent(data_check("IMDN"))
  expect_silent(data_check(c("IMDN", "IMMH")))
})

test_that("data_check() errors on a single invalid code", {
  expect_error(
    data_check("INVALID"),
    regexp = "`data_code` must be one of"
  )
})

test_that("data_check() errors when part of codes are invalid", {
  expect_error(
    data_check(c("IMDN", "INVALID")),
    regexp = "`data_code` must be one of"
  )
})

test_that("data_check() invisibly returns NULL for valid input", {
  res <- data_check("IMDN")
  expect_null(res)
})
