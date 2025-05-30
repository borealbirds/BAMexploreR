##################################################################################
#' Estimate population size from a density raster
#'
#' This function is essentially a wrapper around \code{terra::values},
#' but with an adjustment for converting pixel values from males/ha to males/pixel.
#' It also provides a basic summary of the input raster.
#'
#' @param raster A \code{SpatRaster}. See \code{getlayerNM()} for accessing BAM's raster data.
#'
#' @return A \code{tibble} with four columns: \code{population_size}, \code{mean_density} (per pixel), \code{sd_density}, \code{n_cells}
#'
#'
#' @importFrom tibble tibble
#'
#' @export
#' @examples
#' # download raster for Tennessee Warbler
#' tewa <- getlayerNM("TEWA", "v4", destfile = tempdir())
#' pop_sizeNM(tewa$TEWA) # 124 million

pop_sizeNM <- function(raster){

  # extract all non-NA values from the raster
  pixel_values <- terra::values(raster, mat=FALSE, na.rm=TRUE, dataframe=TRUE)

  # each pixel represents individuals per hectare, but the pixels themselves are 100 ha (1km^2)
  # so need to multiply by 100 ha/pixel to get individuals per pixel
  total_pop <- sum(pixel_values[,1])*100

  # estimate mean density across study area
  mean_density <- mean(pixel_values[,1])
  sd_density <- sd(pixel_values[,1])
  n_cells <- length(pixel_values[,1])

  return(tibble(
    population_size = total_pop,
    mean_density = mean_density,
    sd_density = sd_density,
    n_cells = n_cells
  ))

}



