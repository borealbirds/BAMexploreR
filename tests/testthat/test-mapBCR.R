library(testthat)
library(BAMexploreR)
library(terra)
library(tmap)

# Test if the 'BCR' input is case-sensitive
test_that("mapBCR handles case-sensitive and whitespace issues with version input", {
  expect_error(mapBCR(" V4 "), "Model version doesn't exist.")
  expect_error(mapBCR("v 4"), "Model version doesn't exist.")
})


# Test that the result of v4 is a list, and contains 'subUnits' and 'map'
test_that("mapBCR works with default parameters (v4)", {
  result <- mapBCR(version = "v4")
  expect_type(result, "list")
  expect_true("subUnits" %in% names(result))
  expect_true("map" %in% names(result))
  expect_type(result$subUnits, "character")
  expect_s3_class(result$map, "tmap")
})


# Test that the result of v5 is a list, and contains 'subUnits' and 'map'
test_that("mapBCR returns default when extent is not provided", {
  result <- mapBCR(version = "v5")
  expect_type(result, "list")
  expect_true(length(result$subUnits) > 0)
  expect_s3_class(result$map, "tmap")
})


# Test that an invalid version triggers an error
test_that("mapBCR returns error with invalid version", {
  expect_error(mapBCR(version = "invalid_version"), "Model version doesn't exist.")
})


# Test that the function works with user-defined extent (in 'extdata')
test_that("mapBCR works with user-defined extent (v4)", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
  ext <- ext[1]
  result <- mapBCR(version = "v4", ext = ext)
  expect_type(result, "list")
  expect_true("subUnits" %in% names(result))
  expect_true("map" %in% names(result))
  expect_true(length(result$subUnits) > 0)
  expect_s3_class(result$map, "tmap")
})


# Test that the function cannot works with user-defined extent (in 'extdata') with NULL CRS
test_that("mapBCR returns error with missing CRS in extent", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
  crs(ext) <- ""
  expect_error(mapBCR(version = "v4", ext = ext), "CRS is missing or empty.")
})


# Test that the function shows NULL result on non-valid area
test_that("mapBCR returns empty subUnits when ext has no intersections", {
  non_intersecting_ext <- terra::ext(10000, 20000, 10000, 20000)
  non_intersecting_vect <- terra::vect(non_intersecting_ext, crs = crs("EPSG:4326"))
  result <- mapBCR(version = "v4", ext = non_intersecting_vect)
  expect_equal(length(result$subUnits), 0)
})




