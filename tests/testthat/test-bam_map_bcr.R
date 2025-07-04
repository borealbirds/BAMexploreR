library(testthat)
library(BAMexploreR)
library(terra)
library(tmap)

# Test that an invalid version triggers an error
test_that("bam_map_bcr handles case-sensitive and whitespace issues with version input", {
  expect_error(bam_map_bcr(" V4 "), "Invalid version argument. Must be either 'v4' or 'v5'.")
  expect_error(bam_map_bcr("v 4"), "Invalid version argument. Must be either 'v4' or 'v5'.")
  expect_error(bam_map_bcr(version = "invalid_version"), "Invalid version argument. Must be either 'v4' or 'v5'.")
})


# Test that the result of v4 is a list, and contains a 'map'
test_that("bam_map_bcr works with default parameters (v4)", {
  result <- bam_map_bcr(version = "v4")
  expect_s3_class(result, "tmap")
})


# Test that the function works with user-defined extent (in 'extdata')
test_that("bam_map_bcr works with user-defined extent (v4)", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  ext <- ext[1]
  result <- bam_map_bcr(version = "v4", ext = ext)
  expect_s3_class(result, "tmap")
})


# Test that the function cannot works with user-defined extent (in 'extdata') with NULL CRS
test_that("bam_map_bcr returns error with missing CRS in extent", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  crs(ext) <- ""
  expect_error(bam_map_bcr(version = "v4", ext = ext), "CRS is missing or empty.")
})


# Test that the function shows NULL result on non-valid area
test_that("bam_map_bcr warns if ext does not intersect base BCRs", {
  # Make a spatial extent clearly outside any BCR boundaries
  non_intersecting_ext <- terra::ext(1e6, 2e6, 1e6, 2e6)
  non_intersecting_vect <- terra::vect(non_intersecting_ext, crs = "EPSG:5072")

  expect_warning(
    bam_map_bcr(version = "v4", ext = non_intersecting_vect),
    regexp = "does not intersect"  # Match your actual warning message
  )
})



