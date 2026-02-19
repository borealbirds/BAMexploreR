##################################################################################
#' Estimate population size from a density raster
#'
#' @description This function is essentially a wrapper around \code{terra::values},
#' but with an adjustment for converting pixel values from males/ha to males/pixel.
#' It also provides a basic summary of the input raster.
#'
#' @param raster_list A list of \code{SpatRaster}s. See \code{bam_get_layer()} for accessing BAM's raster data.
#' @param crop_ext SpatVector used to define the extent for the cropping, masking, and grouping of population estimates.
#' @param group Optional character value of column in SpatVector used for grouping population estimates.
#'
#' @return A \code{tibble} with six columns: \code{group}, \code{total_pop}, \code{mean_density} (per pixel), \code{sd_density}, \code{n_cells}, \code{species}
#'
#'
#' @importFrom dplyr left_join mutate row_number rename group_by ungroup summarize all_of n filter
#' @importFrom purrr imap
#' @importFrom stats sd
#' @importFrom tidyselect everything
#' @importFrom terra aggregate
#'
#' @export
#' @examples
#' # download rasters for Tennessee Warbler and Ovenbird
#' rasters <- bam_get_layer(c("TEWA", "OVEN"), "v4", destfile=tempdir())
#'
#' # get summaries of population size
#' bam_pop_size(rasters) # 111 million and 3.5 million, respectively

bam_pop_size <- function(raster_list, crop_ext= NULL, group = NULL){
  # check for valid input
  stopifnot(is.list(raster_list))
  stopifnot(all(purrr::map_lgl(raster_list, ~ inherits(.x, "SpatRaster"))))

  raster_list_mean <- map(raster_list, function(r) {
    lyr_names <- trimws(names(r))
    if ("mean" %in% lyr_names) {
      r[["mean"]]
    } else {
      r  # or return r if you want to keep it as-is
    }
  })

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

  # Check crop_ext area
  if(!is.null(crop_ext)){
    crop_area <- expanse(crop_ext, unit="km")
    if(sum(crop_area) < 100){
      warning(sprintf("The BAM density models are predicted to a resolution of 1 km2. Your area of interest is only %.2f km2. Please consider whether these models are appropriate for your application.", crop_area))
    }
  }

  # Aggregate crop_ext by grouping variable
  crop_ext_grp <- if(!is.null(group)){
    terra::aggregate(crop_ext, by = group)
  } else {crop_ext}

  # define crop function
  crop_raster <- function(r, ext) {
    terra::crop(r, ext, snap = "near", mask = TRUE)
  }

  pop_estimate <- function(raster_i, crop_ext_grp, group, spp) {
    # ——————————————————————————————
    # 1) handle NULL crop_ext_grp by extracting all pixels
    # ——————————————————————————————
    if (is.null(crop_ext_grp)) {
      # Create a data.frame with one row per pixel:
      values <- terra::values(raster_i, mat = FALSE)
      # data.frame with a dummy 'ID' column so the rest of your code works:
      pixel_values <- data.frame(ID = seq_along(values), mean = values)

    } else {
      # normal case
      pixel_values <- terra::extract(raster_i, crop_ext_grp)
      value_col <- setdiff(names(pixel_values), "ID")

      # rename it to "density"
      pixel_values <- pixel_values %>%
        dplyr::rename(mean = all_of(value_col))
    }

    # ——————————————————————————————
    # 2) the rest of your code stays unchanged
    # ——————————————————————————————
    if (!is.na(group)) {
      group_summary <- dplyr::left_join(pixel_values,
                                        as.data.frame(crop_ext_grp) |>
                                          dplyr::mutate(ID = row_number()),
                                        by = "ID") |>
        dplyr::rename(density = mean) |>
        dplyr::filter(!is.na(density)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
        dplyr::summarize(total_pop = round(sum(density)*100, -2),
                         mean_density = round(mean(density), 3),
                         sd_density = round(sd(density), 3),
                         n_cells = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(species = spp) |>
        dplyr::select(species, tidyselect::everything())

      #rename the group column
      colnames(group_summary) <- c("group", colnames(group_summary[2:ncol(group_summary)]))
    } else {
      group_summary <- pixel_values |>
        dplyr::rename(density = mean) |>
        dplyr::filter(!is.na(density)) |>
        dplyr::summarise(
          total_pop   = round(sum(density) * 100, -2),
          mean_density = round(mean(density), 3),
          sd_density   = round(sd(density), 3),
          n_cells      = n()
        ) |>
        dplyr::mutate(group   = NA,
                      species = spp) |>
        dplyr::select(species, group, tidyselect::everything())
    }

    return(group_summary)
  }

  # crop (this will speed things up for small AOIs)
  if (is.null(crop_ext)) {
    crop_list <- raster_list_mean
  } else {
    crop_list <- purrr::imap(raster_list_mean, ~ crop_raster(.x, crop_ext_grp))
  }

  # calculate
  cat("Calculating population size - be patient!\n")
  if(!is.null(group)){
    list_of_summaries <- purrr::imap_dfr(crop_list, ~pop_estimate(
      raster_i = .x,
      spp = .y,
      crop_ext_grp = crop_ext_grp,
      group = group))
  } else {
    list_of_summaries <- purrr::imap_dfr(crop_list, ~pop_estimate(
      raster_i = .x,
      spp = .y,
      crop_ext_grp = crop_ext_grp,
      group = NA))
  }

  # output
  return(list_of_summaries)
}

