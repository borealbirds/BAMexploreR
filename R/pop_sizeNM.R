##################################################################################
#' Estimate population size from a density raster
#'
#' @description This function is essentially a wrapper around \code{terra::values},
#' but with an adjustment for converting pixel values from males/ha to males/pixel.
#' It also provides a basic summary of the input raster.
#'
#' @param raster_list A list of \code{SpatRaster}s. See \code{getlayerNM()} for accessing BAM's raster data.
#'
#' @return A \code{tibble} with five columns: \code{spp}, \code{population_size}, \code{mean_density} (per pixel), \code{sd_density}, \code{n_cells}
#'
#'
#' @importFrom tibble tibble
#' @importFrom purrr list_rbind imap
#'
#'
#' @export
#' @examples
#' # download rasters for Tennessee Warbler and Ovenbird
#' rasters <- getlayerNM(c("TEWA", "OVEN"), "v4", destfile=tempdir())
#'
#' # get summaries of population size
#' pop_sizeNM(rasters) # 111 million and 3.5 million, respectively

pop_sizeNM <- function(raster_list){

  # check for valid input
  stopifnot(is.list(raster_list))
  stopifnot(all(purrr::map_lgl(raster_list, ~ inherits(.x, "SpatRaster"))))

  # define pop estimate function for a single raster
  pop_estimate <- function(raster_i, spp) {

    # extract all non-NA values from the raster
    pixel_values <- terra::values(raster_i, mat=FALSE, na.rm=TRUE, dataframe=TRUE)

    # each pixel represents individuals per hectare, but the pixels themselves are 100 ha (1km^2)
    # so need to multiply by 100 ha/pixel to get individuals per pixel
    total_pop <- sum(pixel_values[,1])*100

    # estimate mean density across study area
    mean_density <- mean(pixel_values[,1])
    sd_density <- sd(pixel_values[,1])
    n_cells <- length(pixel_values[,1])

    return(tibble(
      spp = spp,
      population_size = total_pop,
      mean_density = mean_density,
      sd_density = sd_density,
      n_cells = n_cells))
    }

  list_of_summaries <- purrr::imap(raster_list, pop_estimate)
  pop_summary <- purrr::list_rbind(list_of_summaries)
  return(pop_summary)
}



