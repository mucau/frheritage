test_that("silent_run() returns correct value", {
  expect_identical(silent_run(1 + 1), 2)
})

test_that("silent_run() suppresses messages and warnings", {
  f <- function() { message("msg"); warning("warn"); 42 }
  output <- capture.output({
    res <- silent_run(f())
  })
  expect_identical(res, 42)
  expect_length(output, 0)
})

test_that("silent_run() evaluates in parent environment", {
  x <- 10
  expect_identical(silent_run(x * 2), 20)
})

test_that("silent_run() handles multi-line expressions", {
  res <- silent_run({
    a <- 2; b <- 5
    a * b
  })
  expect_identical(res, 10)
})

test_that("silent_run() works with sf operations", {
  skip_if_not_installed("sf")

  # Load test data
  sevres_path <- system.file("extdata", "sevres.rda", package = "frheritage")
  hauts_de_seine_path <- system.file("extdata", "hauts_de_seine.rda", package = "frheritage")

  skip_if(!file.exists(sevres_path), "sevres.rda not found in inst/extdata")
  skip_if(!file.exists(hauts_de_seine_path), "hauts_de_seine.rda not found in inst/extdata")

  load(sevres_path)
  load(hauts_de_seine_path)

  # Ensure sf objects are available and valid
  expect_true(exists("sevres"))
  expect_true(exists("hauts_de_seine"))
  expect_s3_class(sevres, "sf")
  expect_s3_class(hauts_de_seine, "sf")

  # Test 1: st_cast() inside silent_run
  output_cast <- capture.output({
    res_cast <- silent_run(sf::st_cast(sevres, "POLYGON"))
  })
  expect_s3_class(res_cast, "sf")
  expect_length(output_cast, 0)  # should produce no output

  # Test 2: st_intersection() inside silent_run
  output_inter <- capture.output({
    res_inter <- silent_run(sf::st_intersection(sf::st_transform(sevres, 2154),
                                                sf::st_transform(hauts_de_seine, 2154)))
  })
  expect_s3_class(res_inter, "sf")
  expect_length(output_inter, 0)
})
