library(testthat)
library(BAMexploreR)
library(terra)


  # Test that function throws error if `destfile` is missing
  test_that("getlayerNM throws error if destfile is missing", {
    expect_error(getlayerNM(spList = "BAOR", version = "v4"),
                 "You must provide an output path to store downloaded rasters.")
  })

  test_that("message is shown for invalid species", {
    expect_message(
      getlayerNM(spList = "BBBB", version = "v4", destfile = tempdir()),
      "The following species aren't available for processing"
    )
  })

  # Test error handling for invalid version
  test_that("getlayerNM throws error with invalid version", {
    expect_error(getlayerNM(spList = "BAOR", version = "invalid_version", destfile = tempdir()),
                 "Model version doesn't exist.")
  })

  # Test the basic functionality with mock data
  test_that("getlayerNM downloads raster", {
    result <- getlayerNM(spList = "BAOR", version = "v4", destfile = tempdir())
    expect_type(result, "list")
    expect_true("BAOR" %in% names(result))
    expect_s4_class(result$BAOR, "SpatRaster")
  })

  ## ext
  # Test ext has projection
  test_that("function stops if ext has no CRS", {
    # Create a simple SpatVect without CRS
    library(terra)
    coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)

    p <- vect(list(coords), type = "polygons")
    crs(p) <- ""  # Remove CRS

    # Now expect your function to fail
    expect_error(
      getlayerNM("BAOR", "v4", destfile = tempdir(), crop_ext = p),  # Replace with your function name
      "CRS of crop_ext is missing or empty."
    )
  })
