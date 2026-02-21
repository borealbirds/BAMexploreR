library(testthat)
library(BAMexploreR)

# -------------------------------------------------
# Test: bam_partial_dependence returns a ggplot for continuous predictors
# -------------------------------------------------
test_that("bam_partial_dependence returns ggplot object for continuous predictor", {
  p <- bam_partial_dependence(
    species = "OVEN",
    bcr = "can12",
    predictor = "SCANFIheight_1km",
    version = "v5",
    colour = "purple"
  )
  expect_s3_class(p, "ggplot")
})

# -------------------------------------------------
# Test: bam_partial_dependence runs silently
# -------------------------------------------------
test_that("bam_partial_dependence runs silently without warnings", {
  expect_silent(
    bam_partial_dependence(
      species = "PUFI",
      bcr = "can61",
      predictor = "ERATavesm_1km",
      version = "v5"
    )
  )
})

# -------------------------------------------------
# Test: bam_partial_dependence errors for invalid inputs
# -------------------------------------------------
test_that("bam_partial_dependence stops for invalid species, BCR, or predictor", {
  expect_error(
    bam_partial_dependence(
      species = "NONEXISTENT",
      bcr = "can12",
      predictor = "SCANFIheight_1km"
    )
  )

  expect_error(
    bam_partial_dependence(
      species = "OVEN",
      bcr = 999,
      predictor = "SCANFIheight_1km"
    )
  )

  expect_error(
    bam_partial_dependence(
      species = "OVEN",
      bcr = "can12",
      predictor = "NONEXISTENT"
    )
  )
})

# -------------------------------------------------
# Optional: Test color argument propagates
# -------------------------------------------------
test_that("bam_partial_dependence accepts color argument", {
  p <- bam_partial_dependence(
    species = "PISI",
    bcr = "can60",
    predictor = "StandardDormancy_1km",
    colour = "red"
  )
  expect_s3_class(p, "ggplot")
})
