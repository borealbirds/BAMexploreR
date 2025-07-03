library(testthat)
library(BAMexploreR)


test_that("occurrenceNM works with minimal valid raster_list", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r <- getlayerNM("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  result <- occurrenceNM(r, plot = FALSE)
  expect_type(result, "list")  # or "double", "S4", depending on return
})


test_that("occurrenceNM fails with non-raster input", {
  expect_error(occurrenceNM(raster_list = list(1:10), plot = FALSE))
})


test_that("occurrenceNM returns expected structure", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r1 <- getlayerNM(c("TEWA", "BBWO"), "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  out <- occurrenceNM(raster_list = r1, plot = FALSE)

  expect_s3_class(out$occurrence_summary, "data.frame")
  expect_named(out$occurrence_summary, c("species", "type", "area_km2"))  # adapt to your column names
  expect_equal(nrow(out$occurrence_summary), 4)
  expect_s4_class(out$occurrence_rasters$TEWA, "SpatRaster")
})


test_that("threshold is within raster value range", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r <- getlayerNM("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  out <- occurrenceNM(r, plot = FALSE)

  max_val <- max(terra::values(r$TEWA), na.rm = TRUE)
  expect_true(all(out$threshold >= 0 & out$threshold <= max_val))
})


test_that("different quantile methods yield results", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r <- getlayerNM("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  out <- occurrenceNM(raster_list = r, quantile = "by_lorenz", plot = FALSE)
  out2 <- occurrenceNM(raster_list = r, quantile = 0.8, plot = FALSE)
  expect_true(!is.null(out$occurrence_summary))
  expect_true(!is.null(out2$occurrence_summary))
})
