library(testthat)

test_that("spp_tbl contains species from both available model releases", {
  data("spp_tbl", package = "BAMexploreR")

  expect_equal(nrow(spp_tbl), 151)
  expect_equal(sum(spp_tbl$v4), 143)
  expect_equal(sum(spp_tbl$v5), 149)
  expect_true(all(c("frenchName", "family", "Cavity_Birds") %in% names(spp_tbl)))
  expect_true(all(c("GRAJ", "NESP") %in% spp_tbl$speciesCode[spp_tbl$v4 == 1]))
  expect_true(all(c("BWWA", "CAJA", "DUNL", "RBWO", "RHWO", "ROSA", "WITU", "YBCU") %in%
                    spp_tbl$speciesCode[spp_tbl$v5 == 1]))
})

test_that("species names resolve to release-specific codes", {
  data("spp_tbl", package = "BAMexploreR")

  expect_equal(
    BAMexploreR:::standardize_species_names(
      "Perisoreus canadensis", spp_tbl, version = "v4"
    ),
    "GRAJ"
  )
  expect_equal(
    BAMexploreR:::standardize_species_names(
      "Perisoreus canadensis", spp_tbl, version = "v5"
    ),
    "CAJA"
  )
})

# Test that invalid versions produce an error
test_that("Invalid version argument produces an error", {
  expect_error(bam_spp_list("v6", "species_code"),
               "Invalid version argument. Must be either 'v4' or 'v5'.")
})

# Test that invalid type produces an error
test_that("Invalid type argument produces an error", {
  expect_error(bam_spp_list("v4", "species_color"),
               "Invalid type argument. Must be one of 'speciesCode', 'commonName' or 'scientificName'.")
})

# Test for correct input and expect certain results
test_that("Valid version and type arguments work", {
  result <- bam_spp_list("v4", "speciesCode")
  expect_type(result, "character")
})

test_that("commonName type returns character", {
  result <- bam_spp_list("v4", "commonName")
  expect_type(result, "character")
})

test_that("scientificName type returns character", {
  result <- bam_spp_list("v4", "scientificName")
  expect_type(result, "character")
})

# Test if it handles empty result set gracefully
test_that("Empty species list is handled", {
  # Assuming no matching species codes
  result <- bam_spp_list("v4", "speciesCode")
  expect_true(length(result) == 0 || is.character(result))
})





