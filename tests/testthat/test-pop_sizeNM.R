library(testthat)
library(BAMexploreR)

# Check that stackedbarNM handles filtering by species
test_that("pop_sizeNM handles raster list", {
  rasters <- getlayerNM(c("TEWA", "OVEN"), "v4", destfile=tempdir())
  result <- pop_sizeNM(rasters)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("spp", "population_size", "mean_density", "sd_density", "n_cells") %in% colnames(result)))
})


# Check that stackedbarNM produces a ggplot object
test_that("pop_sizeNM correctly sums pixel values", {
  m <- matrix(10, 2, 2)
  ras <- list(terra::rast(m))
  names(ras) <- "speciesA"

  result <- pop_sizeNM(ras)
  expect_equal(result$population_size, 4000)  # 10 * 4 pixels
  expect_equal(result$mean_density, 10)     # mean of 10s
})


# Test that if groups input is incorrect
test_that("pop_sizeNM works with list of rasters", {
  rasters <- getlayerNM(c("TEWA", "OVEN"), "v4", destfile=tempdir())
  result <- pop_sizeNM(rasters)
  expect_equal(nrow(result), 2)
  expect_true(all(result$spp %in% c("TEWA", "OVEN")))})


# Test that if groups input is missing
test_that("pop_sizeNM handles NAs correctly", {
  m <- matrix(c(1, NA, 3, 5), 2, 2)
  ras <- list(terra::rast(m))
  names(ras) <- "spNA"

  result <- pop_sizeNM(ras)

  expect_equal(result$population_size, sum(c(1, 3, 5))*100)
  expect_equal(result$mean_density, mean(c(1, 3, 5)))
})


test_that("pop_sizeNM throws error on invalid input", {
  expect_error(pop_sizeNM("not a raster"))
})


