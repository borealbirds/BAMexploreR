library(testthat)
library(BAMexploreR)
library(terra)


  # Test that function throws error if `destfile` is missing
  test_that("bam_get_layer throws error if destfile is missing", {
    expect_error(bam_get_layer(spList = "BAOR", version = "v4"),
                 "You must provide an output path to store downloaded rasters.")
  })

  test_that("message is shown for invalid species", {
    expect_error(
      bam_get_layer(spList = "BBBB", version = "v4", destfile = tempdir()),
      "Invalid species in spList: must be in bam_spp_list()"
    )
  })

  # Test error handling for invalid version
  test_that("bam_get_layer throws error with invalid version", {
    expect_error(bam_get_layer(spList = "BAOR", version = "invalid_version", destfile = tempdir()),
                 "Model version doesn't exist.")
  })

  # Test the basic functionality with mock data
  test_that("bam_get_layer downloads raster", {
    result <- bam_get_layer(spList = "BAOR", version = "v4", destfile = tempdir())
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
      bam_get_layer("BAOR", "v4", destfile = tempdir(), crop_ext = p),  # Replace with your function name
      "CRS of crop_ext is missing or empty."
    )
  })


  test_that("bam_get_layer rejects invalid years", {
    # Invalid years should throw an error
    expect_error(
      bam_get_layer(spList = "BAOR", version = "v5", destfile = "tmp.tif", year = 1999),
      "Invalid year"
    )

    expect_error(
      bam_get_layer(spList = "BAOR", version = "v5", destfile = "tmp.tif", year = 2021),
      "Invalid year"
    )
  })

  test_that("bam_get_layer accepts valid years", {
    allowed_years <- c("2000", "2005", "2010", "2015", "2020")

    for (yr in allowed_years) {
      expect_no_error(  # If using testthat < 3.1.0, use expect_silent()
        bam_get_layer(spList = "BTNW", version = "v5", destfile = tempdir(), year = yr)
      )
    }
  })

  test_that("bam_get_layer rejects species not in bam_spp_list", {
    valid_species <- bam_spp_list(version="v5")

    # Pick a valid species and an invalid one
    good_species <- valid_species[1]
    bad_species <- "FAKE_SPECIES"

    # Should fail if there's an invalid species
    expect_error(
      bam_get_layer(
        spList = c(good_species, bad_species),
        version = "v5",
        destfile = tempdir(),
        year = 2020
      ),
      "Invalid species in spList: must be in bam_spp_list()"
    )
  })

  test_that("bam_get_layer accepts only valid species", {
    valid_species <- bam_spp_list(version = "v5")[1:3]  # first 3 valid species

    expect_no_error(  # If using older testthat, use expect_silent()
      bam_get_layer(
        spList = valid_species,
        version = "v5",
        destfile = tempdir(),
        year = 2020
      )
    )
  })
