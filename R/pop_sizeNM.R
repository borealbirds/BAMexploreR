##################################################################################
#' Estimate population size from a density raster
#'
#' @description This function is essentially a wrapper around \code{terra::values},
#' but with an adjustment for converting pixel values from males/ha to males/pixel.
#' It also provides a basic summary of the input raster.
#'
#' @param raster_list A list of \code{SpatRaster}s. See \code{getlayerNM()} for accessing BAM's raster data.
#' @param crop_ext SpatVector used to define the extent for the cropping and grouping of population estimates.
#' @param group Optional character value of column used in SpatVector for grouping population estimates.
#'
#' @return A \code{tibble} with six columns: \code{group} \code{species}, \code{population_size}, \code{mean_density} (per pixel), \code{sd_density}, \code{n_cells}
#'
#'
#' @importFrom dplyr left_join mutate row_number rename group_by ungroup summarize all_of n filter
#' @importFrom purrr imap imp_dfr
#' @importFrom stats sd
#'
#'
#' @export
#' @examples
#' # download rasters for Tennessee Warbler and Ovenbird
#' rasters <- getlayerNM(c("TEWA", "OVEN"), "v4", destfile=tempdir())
#'
#' # get summaries of population size
#' pop_sizeNM(rasters) # 111 million and 3.5 million, respectively

pop_sizeNM <- function(raster_list, crop_ext, group){

  # check for valid input
  stopifnot(is.list(raster_list))
  stopifnot(all(purrr::map_lgl(raster_list, ~ inherits(.x, "SpatRaster"))))

  # Reproject crop_ext
  cat("Aligning projections\n")
  if (!is.null(crop_ext)){
    if(inherits(crop_ext, "SpatVector") || inherits(crop_ext, "SpatRaster") ) {
      if (nchar(crs(crop_ext)) == 0) {
        stop("CRS of crop_ext is missing or empty.")
      }else{
        crop_ext <- terra::project(crop_ext, "EPSG:5072")
      }
    }else{
      stop("crop_ext need to be a SpatVector  or a SpatRaster")
    }
  }

  # Aggregate crop_ext by grouping variable
  crop_ext_grp <- if(!is.na(group)){
    aggregate(crop_ext, by = group)
  } else {crop_ext}

  # define crop function
  crop_raster <- function(r, ext) {
    r_proj <- terra::project(ext, r)
    terra::crop(r, r_proj, snap = "near", mask = TRUE)
  }

  # define pop estimate function for a single raster
  pop_estimate <- function(raster_i, crop_ext_grp, group, spp) {

    # extract for each polygon
    pixel_values <- extract(raster_i, crop_ext_grp)

    # summarize
    # each pixel represents individuals per hectare, but the pixels themselves are 100 ha (1km^2)
    # so need to multiply by 100 ha/pixel to get individuals per pixel
    if(!is.na(group)){

      group_summary <- dplyr::left_join(pixel_values,
                                        as.data.frame(crop_ext_grp) |>
                                          dplyr::mutate(ID = dplyr::row_number()),
                                        by="ID") |>
        dplyr::rename(density = mean) |>
        dplyr::filter(!is.na(density)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
        dplyr::summarize(total_pop = sum(density)*100,
                         mean_density = mean(density),
                         sd_density = sd(density),
                         n_cells = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(species = spp)

      #rename the group column
      colnames(group_summary) <- c("group", colnames(group_summary[2:ncol(group_summary)]))

    } else {

      group_summary <- pixel_values |>
        dplyr::rename(density = mean) |>
        dplyr::filter(!is.na(density)) |>
        dplyr::summarize(total_pop = sum(density)*100,
                         mean_density = mean(density),
                         sd_density = sd(density),
                         n_cells = dplyr::n()) |>
        dplyr::mutate(group = NA,
                      species = spp) |>
        dplyr::select(group, dplyr::everything())

    }

    return(group_summary)

  }

  # crop (this will speed things up for small AOIs)
  crop_list <- purrr::imap(raster_list, ~ crop_raster(.x, crop_ext_grp))

  # calculate
  cat("Calculating population size - be patient!\n")
  if(!is.na(group)){
    list_of_summaries <- purrr::imap_dfr(crop_list, ~pop_estimate(
      raster_i = .x,
      crop_ext_grp = crop_ext_grp,
      group = group,
      spp = .y))
  } else {
    list_of_summaries <- purrr::imap_dfr(crop_list, ~pop_estimate(
      raster_i = .x,
      crop_ext_grp = crop_ext_grp,
      group = NA,
      spp = .y))
  }

  # output
  return(list_of_summaries)
}

