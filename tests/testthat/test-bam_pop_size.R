library(testthat)
library(BAMexploreR)
library(sf)
library(terra)
library(glue)

# Check that bam_pop_size handles filtering by species
test_that("bam_pop_size handles raster list", {
  rasters <- bam_get_layer(c("TEWA", "OVEN"), "v4", destfile=tempdir())
  result <- bam_pop_size(rasters)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("species", "total_pop", "mean_density", "sd_density", "n_cells", "group") %in% colnames(result)))
})


# Check that bam_pop_size correctly sums pixel values
test_that("bam_pop_size correctly sums pixel values", {
  m <- matrix(10, 2, 2)
  ras <- list(terra::rast(m))
  names(ras) <- "speciesA"

  result <- bam_pop_size(ras)
  expect_equal(result$total_pop, 4000)  # 10 * 4 pixels
  expect_equal(result$mean_density, 10)     # mean of 10s
})


# Test if bam_pop_size works with list
test_that("bam_pop_size works with list of rasters", {
  rasters <- bam_get_layer(c("TEWA", "OVEN"), "v4", destfile=tempdir())
  result <- bam_pop_size(rasters)
  expect_equal(nrow(result), 2)
  expect_true(all(result$spp %in% c("TEWA", "OVEN")))
})


# Test that if groups input is missing
test_that("bam_pop_size handles NAs correctly", {
  m <- matrix(c(1, NA, 3, 5), 2, 2)
  ras <- list(terra::rast(m))
  names(ras) <- "spNA"

  result <- bam_pop_size(ras)

  expect_equal(result$total_pop, sum(c(1, 3, 5))*100)
  expect_equal(result$mean_density, mean(c(1, 3, 5)))
})


# Test that it returns an error if input is not a SpatRaster
test_that("bam_pop_size throws error on invalid input", {
  expect_error(bam_pop_size("not a raster"))
})


# Test that Crop work
test_that("bam_pop_size throws error on invalid cropping", {
  rasters <- bam_get_layer("TEWA", "v5", destfile=tempdir())
  aoi_sf <- vect(system.file("extdata", "vignette_poly_5072.shp", package = "BAMexploreR"))
  result <- bam_pop_size(rasters, crop_ext=aoi_sf )

  rast_full <- terra::rast(ext = ext(aoi_sf), res = 1080, crs = crs(aoi_sf))
  values(rast_full) <- 1
  cropped  <- crop(rast_full, aoi_sf)
  manual_n <- sum(!is.na(values(cropped)))
  fn_n       <- result$n_cells[[1]]

  expect_equal(fn_n, manual_n,  tolerance = 0.1, info = glue::glue( "bam_pop_size reported {fn_n} cells, but manual crop has {manual_n} cells"))

})

# Test that grouping work
test_that("bam_pop_size throws error on invalid grouping", {
  # MN, SK  (approx)
  mb_poly <- st_as_sf(st_sfc(st_polygon(list(rbind(c(-102, 60),c(-94, 60),c(-94, 49),c(-102, 49),c(-102, 60))))), crs = 4326)
  sk_poly <- st_as_sf(st_sfc(st_polygon(list(rbind(c(-110, 60),c(-102, 60),c(-102, 49),c(-110, 49),c(-110, 60))))), crs = 4326)
  poly_5072 <- st_transform(rbind(mb_poly, sk_poly), "EPSG:5072")
  sv <- vect(poly_5072)
  sv$id <- c("MB", "SK")

  rasters <- bam_get_layer("TEWA", "v4", destfile=tempdir())
  result <- bam_pop_size(rasters, crop_ext=sv, group = "id" )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})


# Test if bam_pop_size works with list
test_that("bam_pop_size works with list of rasters while croping and grouping", {
  mb_poly <- st_as_sf(st_sfc(st_polygon(list(rbind(c(-102, 60),c(-94, 60),c(-94, 49),c(-102, 49),c(-102, 60))))), crs = 4326)
  sk_poly <- st_as_sf(st_sfc(st_polygon(list(rbind(c(-110, 60),c(-102, 60),c(-102, 49),c(-110, 49),c(-110, 60))))), crs = 4326)
  poly_5072 <- st_transform(rbind(mb_poly, sk_poly), "EPSG:5072")
  sv <- vect(poly_5072)
  sv$id <- c("MB", "SK")

  rasters <- bam_get_layer(c("TEWA", "OVEN"), "v4", destfile=tempdir())
  result <- bam_pop_size(rasters, crop_ext=sv, group = "id")
  expect_equal(nrow(result), 4)
  expect_true(all(result$species %in% c("TEWA", "OVEN")))
})
