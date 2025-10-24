test_that("ids_to_codes() returns correct codes for exact matches", {
  expect_identical(ids_to_codes("Immeubles classés ou inscrits"), "IMMH")
  expect_identical(ids_to_codes("Protection au titre des abords de monuments historiques"), "PAMH")
  expect_identical(ids_to_codes("Domaines nationaux"), "IMDN")
})

test_that("ids_to_codes() handles case-insensitive matches", {
  expect_identical(ids_to_codes("immeubles classés ou inscrits"), "IMMH")
  expect_identical(ids_to_codes("DOMAINES NATIONAUX"), "IMDN")
})

test_that("ids_to_codes() selects the most specific pattern when multiple match", {
  expect_identical(ids_to_codes("Abord d'un monument historique"), "PAMH")
})

test_that("ids_to_codes() returns NA when no pattern matches", {
  expect_true(is.na(ids_to_codes("Unknown heritage site")))
  expect_true(all(is.na(ids_to_codes(c("", "random text")))))
})

test_that("ids_to_codes() works for a vector of mixed titles", {
  titles <- c(
    "Immeubles classés ou inscrits",
    "Sites patrimoniaux remarquables",
    "Non-matching title",
    "Domaines nationaux"
  )
  codes <- ids_to_codes(titles)
  expect_identical(codes, c("IMMH", "SIPR", NA_character_, "IMDN"))
})

test_that("ids_to_codes() handles very long text containing a pattern", {
  long_text <- paste(rep("bla ", 50), "Sites inscrits", rep("bla ", 50), collapse = "")
  expect_identical(ids_to_codes(long_text), "SICI")
})
