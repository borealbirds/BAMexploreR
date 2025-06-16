library(testthat)
library(BAMexploreR)
library(terra)

# Test that an invalid version triggers an error
test_that("getBCR handles case-sensitive and whitespace issues with version input", {
  expect_error(getBCR(" V4 "), "Invalid version argument. Must be either 'v4' or 'v5'.")
  expect_error(getBCR("v 4"), "Invalid version argument. Must be either 'v4' or 'v5'.")
  expect_error(getBCR(version = "invalid_version"), "Invalid version argument. Must be either 'v4' or 'v5'.")
})


# Test that the result of v4 is a list, and contains 'subUnits' and 'map'
test_that("getBCR works with default parameters (v4)", {
  result <- getBCR(version = "v4")
  expect_type(result, "character")
})


# Test that the result of v5 is a list, and contains 'subUnits' and 'map'
test_that("getBCR returns default when extent is not provided", {
  result <- getBCR(version = "v5")
  expect_type(result, "character")
  expect_true(identical(result, c("can3", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                                           "can40", "can41", "can42", "can60", "can61", "can70", "can71", "can72", "can80",
                                           "can81", "can82", "usa2", "usa5", "usa9", "usa10", "usa11", "usa12", "usa13",
                                           "usa14", "usa23", "usa28", "usa30", "usa40", "usa43", "usa41423"))
  )
  result <- getBCR(version = "v4")
  expect_true(identical(result, c("can4", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                                  "can60", "can61", "can70", "can71", "can80", "can81", "can82", "can83"))
  )
})


# Test that the function works with user-defined extent (in 'extdata')
test_that("getBCR works with user-defined extent (v4)", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  ext <- ext[1]
  result <- getBCR(version = "v4", ext = ext)
  expect_type(result, "character")
  expect_true(any(result %in% "can4", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                  "can60", "can61", "can70", "can71", "can80", "can81", "can82", "can83"))
})


# Test that the function cannot works with user-defined extent (in 'extdata') with NULL CRS
test_that("getBCR returns error with missing CRS in extent", {
  ext <- vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  crs(ext) <- ""
  expect_error(getBCR(version = "v4", ext = ext), "CRS is missing or empty.")
})


# Test that the function shows NULL result on non-valid area
test_that("getBCR returns empty subUnits when ext has no intersections", {
  non_intersecting_ext <- terra::ext(10000, 20000, 10000, 20000)
  non_intersecting_vect <- terra::vect(non_intersecting_ext, crs = crs("EPSG:5072"))
  result <- getBCR(version = "v4", ext = non_intersecting_vect)
  expect_equal(length(result), 0)
})




