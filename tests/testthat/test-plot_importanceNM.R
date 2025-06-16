library(testthat)
library(BAMexploreR)

# Check that plot_importanceNM return a ggplot
test_that("plot_importanceNM returns ggplot object when plot = TRUE", {
  p <- plot_importanceNM(species = "TEWA", plot = TRUE)
  expect_s3_class(p, "ggplot")
})



# Confirm it doesn’t throw unexpected messages or warnings
test_that("plot_importanceNM runs silently", {
  expect_silent(plot_importanceNM(species = "TEWA", plot = TRUE))
})


# Test plot = FALSE
test_that("plot_importanceNMNM returns a tibble when plot = FALSE", {
  out <- plot_importanceNM(species = "TEWA", plot = FALSE)
  expect_s3_class(out, "tbl_df") # Or data.frame depending on your code
})


# Test that grouping works as expected (e.g., spp and bcr).
test_that("plot_importanceNM works with different group options", {
  p1 <- plot_importanceNM(group = "spp")
  p2 <- plot_importanceNM(group = "bcr")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})


# Test invalid inputs
test_that("plot_importanceNM fails with invalid group", {
  expect_error(plot_importanceNM(group = "nonsense"))
  expect_error(plot_importanceNM(species = "lolo", group = "spp"))
  expect_error(plot_importanceNM(group = "spp", bcr = "lolo"))
})


# Test if all species are plotted
test_that("plot_importance has two groups when two species are passed", {
  p <- plot_importanceNM(species = c("ALFL", "OSFL"), plot = TRUE)
  plot_data <- p$data

  # Check that the grouping column has two levels
  expect_true(all(c("ALFL", "OSFL") %in% unique(plot_data$spp)))
  expect_equal(length(unique(plot_data$spp)), 2)
})
