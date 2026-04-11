test_that("get_heritage() fails when data_code has length > 1", {
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)

  expect_error(
    get_heritage(sevres, data_code = c("IMMH", "SICI")),
    "`data_code` must be a single heritage code"
  )
})

test_that("get_heritage() errors when data_code is invalid", {
  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)

  expect_error(
    get_heritage(sevres, data_code = "INVALID_CODE"),
    "must be one of"
  )
})

test_that("get_heritage() retrieves one heritage data correctly", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  sevres_path <- system.file("extdata/sevres.rda", package = "frheritage")
  load(sevres_path)
  expect_s3_class(sevres, "sf")


  result <- get_heritage(x = sevres,
                         data_code = "IMMH",
                         buffer = 500,
                         crs = 2154,
                         verbose = FALSE)


  expect_s3_class(result, "sf")
  expect_true(nrow(result) > 0)
  expect_true("geometry" %in% names(result))
  expect_identical(sf::st_crs(result)$epsg, as.integer(2154))
})

test_that("get_heritage() processes pts, lines and polygons independently", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  crs <- 4326

  # --- Points ----
  pts <- sf::st_sf(
    type = "point",
    name = c("Paris A", "Paris B", "Rouen A", "Le Mans A"),
    geometry = sf::st_sfc(
      sf::st_point(c(2.3522, 48.8566)),
      sf::st_point(c(2.3600, 48.8600)),
      sf::st_point(c(1.0993, 49.4431)),
      sf::st_point(c(0.2000, 48.0061))
    ),
    crs = crs
  )

  # --- Lines ----
  lines <- sf::st_sf(
    type = "line",
    name = c("Paris line", "Rouen line"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(
        2.30, 48.85,
        2.40, 48.87,
        2.35, 48.90
      ), ncol = 2, byrow = TRUE)),

      sf::st_linestring(matrix(c(
        1.05, 49.43,
        1.10, 49.45,
        1.15, 49.46
      ), ncol = 2, byrow = TRUE))
    ),
    crs = crs
  )

  # --- Polygons ----
  polys <- sf::st_sf(
    type = "polygon",
    name = c("Paris area", "Rouen area", "Le Mans area", "Paris buffer", "Perche area"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        2.33, 48.84,
        2.38, 48.84,
        2.38, 48.88,
        2.33, 48.88,
        2.33, 48.84
      ), ncol = 2, byrow = TRUE))),

      sf::st_polygon(list(matrix(c(
        1.05, 49.42,
        1.12, 49.42,
        1.12, 49.47,
        1.05, 49.47,
        1.05, 49.42
      ), ncol = 2, byrow = TRUE))),

      sf::st_polygon(list(matrix(c(
        0.18, 48.00,
        0.25, 48.00,
        0.25, 48.03,
        0.18, 48.03,
        0.18, 48.00
      ), ncol = 2, byrow = TRUE))),

      sf::st_polygon(list(matrix(c(
        2.34, 48.85,
        2.41, 48.85,
        2.41, 48.90,
        2.34, 48.90,
        2.34, 48.85
      ), ncol = 2, byrow = TRUE))),

      sf::st_polygon(list(matrix(c(
        0.75, 47.85,
        0.95, 47.85,
        0.95, 47.98,
        0.75, 47.98,
        0.75, 47.85
      ), ncol = 2, byrow = TRUE)))
    ),
    crs = crs
  )

  # --- list of inputs ----
  inputs <- list(
    points = pts,
    lines = lines,
    polygons = polys
  )

  # --- loop ----
  for (nm in names(inputs)) {

    x <- inputs[[nm]]

    res <- get_heritage(
      x = x,
      data_code = "IMMH",
      verbose = FALSE
    )

    expect_s3_class(res, "sf")
    expect_true(is.data.frame(res))
    expect_true(nrow(res) >= 0)
  }
})
