library(testthat)
library(BAMexploreR)

# Check that bam_predictor_barchart handles filtering by species
test_that("bam_predictor_barchart handles filtering by species", {
  result <- bam_predictor_barchart(species = "ALFL", groups = c("spp", "var_class"), plot = FALSE)
  expect_true(all(result$spp == "ALFL"))
})



# Check that bam_predictor_barchart produces a ggplot object
test_that("bam_predictor_barchart produces a ggplot object when plot = TRUE", {
  result <- bam_predictor_barchart(groups = c("var_class", "bcr"), plot = TRUE)
  expect_s3_class(result, "ggplot")
})


# Test that proportions calculated correctly
test_that("bam_predictor_barchart calculates proportions correctly", {
  result <- bam_predictor_barchart(groups = c("var_class", "bcr"), plot = FALSE)
  proportions <- result %>%
    dplyr::group_by(var_class) %>%
    dplyr::summarise(total_prop = sum(prop))
  expect_true(all(proportions$total_prop <= 1))
})


# Test that if groups input is incorrect
test_that("bam_predictor_barchart throws error if groups input is missing", {
  expect_error(bam_predictor_barchart(groups = c("var_class", "bcrs"), plot = FALSE))
})


# Test that if groups input is missing
test_that("bam_predictor_barchart throws error if groups input is missing", {
  expect_error(bam_predictor_barchart(groups = "var_class", plot = FALSE))
})

