library(testthat)
library(BAMexploreR)
library(terra)

test_that("mapBCR works with default parameters (v4)", {
  result <- mapBCR(version = "v4")

  # Check that the result is a list
  expect_type(result, "list")

  # Check that the list contains 'subUnits' and 'map'
  expect_true("subUnits" %in% names(result))
  expect_true("map" %in% names(result))

  # Check that 'subUnits' is a character vector
  expect_type(result$subUnits, "character")

  # Check that the 'map' is a tmap object
  expect_s3_class(result$map, "tmap")
})

test_that("mapBCR works with user-defined extent (v4)", {
  # Create a simple SpatVector to use as the extent
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
  ext <- ext[1]  # Use just one subunit for testing

  result <- mapBCR(version = "v4", ext = ext)

  # Check that the result is a list
  expect_type(result, "list")

  # Check that the list contains 'subUnits' and 'map'
  expect_true("subUnits" %in% names(result))
  expect_true("map" %in% names(result))

  # Check that 'subUnits' contains the correct subunit
  expect_true(length(result$subUnits) > 0)

  # Check that 'map' is a tmap object
  expect_s3_class(result$map, "tmap")
})

test_that("mapBCR returns error with invalid version", {
  # Test that an invalid version triggers an error
  expect_error(mapBCR(version = "invalid_version"), "Model version doesn't exist.")
})

test_that("mapBCR returns error with missing CRS in extent", {
  # Create an extent without CRS
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
  crs(ext) <- ""  # Remove the CRS

  expect_error(mapBCR(version = "v4", ext = ext), "CRS is missing or empty.")
})

test_that("mapBCR returns default when extent is not provided", {
  # Test the default case when extent is not provided
  result <- mapBCR(version = "v5")

  # Check that the result is a list
  expect_type(result, "list")

  # Check that 'subUnits' is not empty
  expect_true(length(result$subUnits) > 0)

  # Check that the map is a tmap object
  expect_s3_class(result$map, "tmap")
})
