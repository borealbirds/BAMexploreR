library(testthat)
library(BAMexploreR)
library(googledrive)
library(terra)

# Mock Google Drive functions for testing purposes
with_mock(
  `googledrive::drive_ls` = function(...) {
    # Return a mock list of files available on Google Drive
    return(data.frame(
      name = c("pred-BAOR-CAN-Mean.tif", "pred-CAWA-CAN-Mean.tif"),
      id = c("1a2b3c", "4d5e6f"),
      stringsAsFactors = FALSE
    ))
  },
  `googledrive::drive_download` = function(file, path, ...) {
    # Mock the downloading process by creating a temporary file
    writeRaster(rast(matrix(runif(100), 10, 10)), path)
  },

  # Test that function throws error if `destfile` is missing
  test_that("getlayerNM throws error if destfile is missing", {
    expect_error(getlayerNM(spList = "BAOR", version = "v4", layer = "mean"),
                 "You must provide an output path to store downloaded rasters.")
  }),

  # Test that function throws error if `crop = TRUE` and `ext` is missing
  test_that("getlayerNM throws error if crop = TRUE and ext is missing", {
    expect_error(getlayerNM(spList = "BAOR", version = "v4", destfile = tempdir(), layer = "mean", crop = TRUE),
                 "You need to provide a SpatRast, SpatVect or a specific BCR unit in order to crop.")
  }),

  # Test the basic functionality with mock data
  test_that("getlayerNM downloads raster without cropping", {
    result <- getlayerNM(spList = "BAOR", version = "v4", destfile = tempdir(), layer = "mean", crop = FALSE)

    # Check that the result is a list
    expect_type(result, "list")

    # Check that the raster is downloaded for the species
    expect_true("BAOR" %in% names(result))

    # Check if the downloaded raster is a SpatRaster
    expect_s4_class(result$BAOR, "SpatRaster")
  }),

  # Test that function correctly handles the crop = TRUE case with extent
  test_that("getlayerNM handles cropping with a SpatVector", {
    ext <- vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))[1]  # Use one subunit for testing
    result <- getlayerNM(spList = "BAOR", version = "v4", destfile = tempdir(), layer = "mean", crop = TRUE, ext = ext)

    # Check if the result contains the cropped raster
    expect_type(result, "list")
    expect_true("BAOR" %in% names(result))
    expect_s4_class(result$BAOR, "SpatRaster")
  }),

  # Test error handling for invalid species
  test_that("getlayerNM warns about unavailable species", {
    # Check that a warning is given for species not in the list
    expect_warning(result <- getlayerNM(spList = c("INVALID", "BAOR"), version = "v4", destfile = tempdir(), layer = "mean"),
                   "The following species aren't available for processing: INVALID")

    # Check that BAOR is still processed
    expect_true("BAOR" %in% names(result))
  }),

  # Test error handling for invalid version
  test_that("getlayerNM throws error with invalid version", {
    expect_error(getlayerNM(spList = "BAOR", version = "invalid_version", destfile = tempdir(), layer = "mean"),
                 "Model version doesn't exist.")
  })
)
