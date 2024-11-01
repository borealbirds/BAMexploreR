library(testthat)
library(BAMexploreR)

# Set up a mock dataset for testing
mock_data <- data.frame(
  var = c("SCANFI_1km", "SCANFIprcD_5x5"),
  rel.inf = c(15.8, 11.0),
  var_class = c("Landcover", "Biomass"),
  spp = c("ALFL", "ALFL"),
  bcr = c("can10", "can10"),
  boot = c(1, 1),
  common_name = c("Alder Flycatcher", "Alder Flycatcher"),
  sci_name = c("Empidonax alnorum", "Empidonax alnorum"),
  spp6 = c("EMPALN", "EMPALN"),
  file_name = c("ALFL_can10_1.R", "ALFL_can10_1.R")
)



# Check that stackedbarNM handles filtering by species
test_that("stackedbarNM handles filtering by species", {
  result <- stackedbarNM(data = mock_data, species = "Alder Flycatcher", groups = c("common_name", "var_class"), plot = FALSE)
  expect_true(all(result$common_name == "Alder Flycatcher"))
})



# Check that stackedbarNM produces a ggplot object
test_that("stackedbarNM produces a ggplot object when plot = TRUE", {
  result <- stackedbarNM(data = mock_data, groups = c("var_class", "bcr"), plot = TRUE)
  expect_s3_class(result, "ggplot")
})


# Should warn when traits is not NULL or a data.frame
test_that("stackedbarNM returns a warning for invalid traits input", {
  expect_warning(stackedbarNM(data = mock_data, traits = "invalid_traits", groups = c("var_class", "bcr")))
})


# Test that proportions calculated correctly
test_that("stackedbarNM calculates proportions correctly", {
  result <- stackedbarNM(data = mock_data, groups = c("var_class", "bcr"), plot = FALSE)
  proportions <- result %>%
    dplyr::group_by(var_class) %>%
    dplyr::summarise(total_prop = sum(prop))
  expect_true(all(proportions$total_prop <= 1))
})


# Test that if groups input is incorrect
test_that("stackedbarNM throws error if groups input is missing", {
  expect_error(stackedbarNM(data = mock_data))
})
