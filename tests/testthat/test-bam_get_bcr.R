library(testthat)
library(BAMexploreR)
library(terra)

# Test that an invalid version triggers an error
test_that("bam_get_bcr handles case-sensitive and whitespace issues with version input", {
  expect_error(bam_get_bcr(" V4 "), "Invalid version argument. Must be either 'v4' or 'v5'.")
  expect_error(bam_get_bcr("v 4"), "Invalid version argument. Must be either 'v4' or 'v5'.")
  expect_error(bam_get_bcr(version = "invalid_version"), "Invalid version argument. Must be either 'v4' or 'v5'.")
})


# Test that the result of v4 is a list, and contains 'subUnits' and 'map'
test_that("bam_get_bcr works with default parameters (v4)", {
  result <- bam_get_bcr(version = "v4")
  expect_type(result, "character")
})


# Test that the result of v5 is a list, and contains 'subUnits' and 'map'
test_that("bam_get_bcr returns default when extent is not provided", {
  result <- bam_get_bcr(version = "v5")
  expect_type(result, "character")
  expect_true(identical(result, c("can3", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                                           "can40", "can41", "can42", "can60", "can61", "can70", "can71", "can72", "can80",
                                           "can81", "can82"))
  )
  result <- bam_get_bcr(version = "v4")
  expect_true(identical(result, c("can4", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                                  "can60", "can61", "can70", "can71", "can80", "can81", "can82", "can83"))
  )
})


# Test that the function works with user-defined extent (in 'extdata')
test_that("bam_get_bcr works with user-defined extent (v4)", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  ext <- ext[1]
  result <- bam_get_bcr(version = "v4", ext = ext)
  expect_type(result, "character")
  expect_true(any(result %in% "can4", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                  "can60", "can61", "can70", "can71", "can80", "can81", "can82", "can83"))
})


# Test that the function cannot works with user-defined extent (in 'extdata') with NULL CRS
test_that("bam_get_bcr returns error with missing CRS in extent", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  crs(ext) <- ""
  expect_error(bam_get_bcr(version = "v4", ext = ext), "CRS is missing or empty.")
})


# Test that the function shows NULL result on non-valid area
test_that("bam_get_bcr returns empty subUnits when ext has no intersections", {
  non_intersecting_ext <- terra::ext(10000, 20000, 10000, 20000)
  non_intersecting_vect <- terra::vect(non_intersecting_ext, crs = crs("EPSG:5072"))
  result <- bam_get_bcr(version = "v4", ext = non_intersecting_vect)
  expect_equal(length(result), 0)
})




