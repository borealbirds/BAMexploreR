library(testthat)
library(BAMexploreR)

# Check that stackedbarNM handles filtering by species
test_that("stackedbarNM handles filtering by species", {
  result <- stackedbarNM(species = "ALFL", groups = c("spp", "var_class"), plot = FALSE)
  expect_true(all(result$spp == "ALFL"))
})



# Check that stackedbarNM produces a ggplot object
test_that("stackedbarNM produces a ggplot object when plot = TRUE", {
  result <- stackedbarNM(groups = c("var_class", "bcr"), plot = TRUE)
  expect_s3_class(result, "ggplot")
})


# Test that proportions calculated correctly
test_that("stackedbarNM calculates proportions correctly", {
  result <- stackedbarNM(groups = c("var_class", "bcr"), plot = FALSE)
  proportions <- result %>%
    dplyr::group_by(var_class) %>%
    dplyr::summarise(total_prop = sum(prop))
  expect_true(all(proportions$total_prop <= 1))
})


# Test that if groups input is incorrect
test_that("stackedbarNM throws error if groups input is missing", {
  expect_error(stackedbarNM(groups = c("var_class", "bcrs"), plot = FALSE))
})


# Test that if groups input is missing
test_that("stackedbarNM throws error if groups input is missing", {
  expect_error(stackedbarNM(groups = "var_class", plot = FALSE))
})

