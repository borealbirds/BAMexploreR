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
    writeRaster(rast(matrix(runif(100), 10, 10)), path, overwrite = TRUE)
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
    expect_type(result, "list")
    expect_true("BAOR" %in% names(result))
    expect_s4_class(result$BAOR, "SpatRaster")
  }),



  # Test error handling for invalid version
  test_that("getlayerNM throws error with invalid version", {
    expect_error(getlayerNM(spList = "BAOR", version = "invalid_version", destfile = tempdir(), layer = "mean"),
                 "Model version doesn't exist.")
  })
)
