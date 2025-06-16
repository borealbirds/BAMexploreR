library(testthat)
library(BAMexploreR)


test_that("occurrenceNM works with minimal valid raster_list", {
  r <- terra::rast(matrix(runif(100), 10, 10))
  result <- occurrenceNM(raster_list = list(r), plot = FALSE)
  expect_type(result, "list")  # or "double", "S4", depending on return
})


test_that("occurrenceNM fails with non-raster input", {
  expect_error(occurrenceNM(raster_list = list(1:10), plot = FALSE))
})


test_that("occurrenceNM returns expected structure", {
  r1 <- terra::rast(matrix(runif(100), 10, 10))
  r2 <- terra::rast(matrix(runif(100), 10, 10))
  out <- occurrenceNM(raster_list = list(r1, r2), plot = FALSE)

  expect_s3_class(out$population_summary, "data.frame")
  expect_named(out$population_summary, c("spp", "population_size", "mean_density", "sd_density", "n_cells", "type"))  # adapt to your column names
  expect_equal(nrow(out$population_summary), 4)
})


test_that("threshold is within raster value range", {
  r <- terra::rast(matrix(runif(100, min=0, max=1), 10, 10))
  out <- occurrenceNM(raster_list = list(r), plot = FALSE)

  max_val <- max(terra::values(r), na.rm = TRUE)
  expect_true(all(out$threshold >= 0 & out$threshold <= max_val))
})


test_that("plot is a ggplot object when plot = TRUE", {
  r <- terra::rast(matrix(runif(100), 10, 10))
  p <- occurrenceNM(raster_list = list(r), plot = TRUE)
  expect_s3_class(p, "ggplot")
})


test_that("different quantile methods yield results", {
  r <- terra::rast(matrix(runif(100), 10, 10))
  q1 <- occurrenceNM(raster_list = list(r), quantile = "by_lorenz", plot = FALSE)
  expect_true(!is.null(q1$threshold))
})
