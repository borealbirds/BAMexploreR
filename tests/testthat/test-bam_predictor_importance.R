library(testthat)
library(BAMexploreR)

# Check that bam_predictor_importance return a ggplot
test_that("bam_predictor_importance returns ggplot object when plot = TRUE", {
  p <- bam_predictor_importance(species = "TEWA", plot = TRUE)
  expect_s3_class(p, "ggplot")
})



# Confirm it doesn’t throw unexpected messages or warnings
test_that("bam_predictor_importance runs silently", {
  expect_silent(bam_predictor_importance(species = "TEWA", plot = TRUE))
})


# Test plot = FALSE
test_that("bam_predictor_importanceNM returns a tibble when plot = FALSE", {
  out <- bam_predictor_importance(species = "TEWA", plot = FALSE)
  expect_s3_class(out, "tbl_df") # Or data.frame depending on your code
})


# Test that grouping works as expected (e.g., spp and bcr).
test_that("bam_predictor_importance works with different group options", {
  p1 <- bam_predictor_importance(group = "spp")
  p2 <- bam_predictor_importance(group = "bcr")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})


# Test invalid inputs
test_that("bam_predictor_importance fails with invalid group", {
  expect_error(bam_predictor_importance(group = "nonsense"))
  expect_error(bam_predictor_importance(species = "lolo", group = "spp"))
  expect_error(bam_predictor_importance(group = "spp", bcr = "lolo"))
})


# Test if all species are plotted
test_that("plot_importance has two groups when two species are passed", {
  p <- bam_predictor_importance(species = c("ALFL", "OSFL"), plot = TRUE)
  plot_data <- p$data

  # Check that the grouping column has two levels
  expect_true(all(c("ALFL", "OSFL") %in% unique(plot_data$spp)))
  expect_equal(length(unique(plot_data$spp)), 2)
})
