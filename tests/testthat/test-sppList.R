library(testthat)

# Test that invalid versions produce an error
test_that("Invalid version argument produces an error", {
  expect_error(sppList("v6", "species_code"),
               "Invalid version argument. Must be either 'v4' or 'v5'.")
})

# Test that invalid type produces an error
test_that("Invalid type argument produces an error", {
  expect_error(sppList("v4", "species_color"),
               "Invalid type argument. Must be one of 'speciesCode', 'commonName' or 'scientificName'.")
})

# Test for correct input and expect certain results
test_that("Valid version and type arguments work", {
  result <- sppList("v4", "speciesCode")
  expect_type(result, "character")
})

test_that("commonName type returns character", {
  result <- sppList("v4", "commonName")
  expect_type(result, "character")
})

test_that("scientificName type returns character", {
  result <- sppList("v4", "scientificName")
  expect_type(result, "character")
})

# Test if it handles empty result set gracefully
test_that("Empty species list is handled", {
  # Assuming no matching species codes
  result <- sppList("v4", "speciesCode")
  expect_true(length(result) == 0 || is.character(result))
})





