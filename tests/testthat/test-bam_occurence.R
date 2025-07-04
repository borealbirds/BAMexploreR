library(testthat)
library(BAMexploreR)


test_that("bam_occurrence works with minimal valid raster_list", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r <- bam_get_layer("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  result <- bam_occurrence(r, plot = FALSE)
  expect_type(result, "list")  # or "double", "S4", depending on return
})


test_that("bam_occurrence fails with non-raster input", {
  expect_error(bam_occurrence(raster_list = list(1:10), plot = FALSE))
})


test_that("bam_occurrence returns expected structure", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r1 <- bam_get_layer(c("TEWA", "BBWO"), "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  out <- bam_occurrence(raster_list = r1, plot = FALSE)

  expect_s3_class(out$occurrence_summary, "data.frame")
  expect_named(out$occurrence_summary, c("species", "type", "area_km2"))  # adapt to your column names
  expect_equal(nrow(out$occurrence_summary), 4)
  expect_s4_class(out$occurrence_rasters$TEWA, "SpatRaster")
})


test_that("threshold is within raster value range", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r <- bam_get_layer("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  out <- bam_occurrence(r, plot = FALSE)

  max_val <- max(terra::values(r$TEWA), na.rm = TRUE)
  expect_true(all(out$threshold >= 0 & out$threshold <= max_val))
})


test_that("different quantile methods yield results", {
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  r <- bam_get_layer("TEWA", "v5",  crop_ext = aoi_sf, destfile = tempdir(), year = "2020")
  out <- bam_occurrence(raster_list = r, quantile = "by_lorenz", plot = FALSE)
  out2 <- bam_occurrence(raster_list = r, quantile = 0.8, plot = FALSE)
  expect_true(!is.null(out$occurrence_summary))
  expect_true(!is.null(out2$occurrence_summary))
})
