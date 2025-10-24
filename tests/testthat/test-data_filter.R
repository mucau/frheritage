test_that("data_filter() returns subset with valid inputs", {
  skip_if_not_exists <- function() skip("frheritage::all_ids not available for testing")
  if (!exists("all_ids", envir = asNamespace("frheritage"))) skip_if_not_exists()

  # Pick a sample code and department from frheritage::all_ids
  sample_row <- frheritage::all_ids[1, , drop = FALSE]
  code_val <- sample_row$code
  dept_val <- sample_row$departement

  res <- data_filter(department = dept_val, data_code = code_val)
  expect_s3_class(res, "data.frame")
  expect_true(all(res$code %in% code_val))
})

test_that("data_filter() errors on invalid data_code", {
  expect_error(
    data_filter(department = "75", data_code = "INVALID"),
    regexp = "`data_code` must be one of"
  )
})

test_that("data_filter() uses all_ids if ids is NULL", {
  sample_row <- frheritage::all_ids[1, , drop = FALSE]
  code_val <- sample_row$code
  dept_val <- sample_row$departement

  res <- data_filter(ids = NULL, department = dept_val, data_code = code_val)
  expect_s3_class(res, "data.frame")
})

test_that("data_filter() excludes 'departement' column in output", {
  sample_row <- frheritage::all_ids[1, , drop = FALSE]
  code_val <- sample_row$code
  dept_val <- sample_row$departement

  res <- data_filter(department = dept_val, data_code = code_val)
  expect_false("departement" %in% colnames(res))
})

