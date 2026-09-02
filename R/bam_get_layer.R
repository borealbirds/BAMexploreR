##################################################################################
#' Download BAM Landbird Density & Habitat model rasters
#'
#' @param version Specifies the model release: \code{"v5"} for the current
#'   models or \code{"v4"} for the archived models. Default is "v5".
#'
#' @param spList A \code{vector} of species to be downloaded.
#'
#' @param destfile A \code{character} indicating output path where the downloaded file is saved.
#'
#' @param crop_ext A \code{SpatVector} or A \code{SpatRaster} used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: \code{bam_map_bcr("v4")} or \code{bam_map_bcr("v5")}
#'
#' @param bcrNM A \code{vector} representing the BCR subunit name according to model version. Default is "Canada".
#'   If a \code{crop_ext} has been provided, the argument will be ignored.
#'
#' @param year A \code{character} specifying the prediction year for the current
#'   models. Only \code{"2020"} is currently available for public download and
#'   is the default. Predictions at five-year intervals from 1995 to 2015 are
#'   available by request for v5 from \email{bamp@ualberta.ca}.If a \code{version} is v4,
#'   the argument will be ignored.
#'
#' @return A list of \code{SpatRaster} objects. In addition to returning these objects,
#' the function also downloads raster files to the directory specified by \code{destfile},
#' as a side-effect.
#'
#' @examples
#' bird <- bam_get_layer( "v4", "TEWA", destfile = tempdir())
#'
#' @author Melina Houle
#' @docType methods
#' @rdname bam_get_layer
#' @export
#'
#' @importFrom dplyr pull
#' @importFrom httr GET content
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values crs writeRaster same.crs expanse
#' @importFrom stats setNames
#'
bam_get_layer <- function(version= "v5", spList, destfile, crop_ext = NULL, bcrNM= "Canada",  year = "2020") {

  # Valid Model versions
  if (!version %in% c("v4", "v5")) {
    stop("Model version doesn't exist.")
  }

  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }

  # Valid year
  if (is.null(year)){
    if(version == "v5"){
      year <- c("2020")
    }
  }
  # Check crop_ext area
  if(!is.null(crop_ext)){
    crop_area <- expanse(crop_ext, unit="km")
    if(sum(crop_area) < 100){
      warning(sprintf("The BAM density models are predicted to a resolution of 1 km2. Your area of interest is only %.2f km2. Please consider whether these models are appropriate for your application.", crop_area))
    }
    if(inherits(crop_ext, "SpatVector") || inherits(crop_ext, "SpatRaster") ) {
      if (nchar(crs(crop_ext)) == 0) {
        stop("CRS of crop_ext is missing or empty.")
      }else{
        if (crs(crop_ext, describe = TRUE)$code != 3978) {
          crop_ext <- terra::project(crop_ext, "EPSG:3978")
        }
      }
    }else{
      stop("crop_ext need to be a SpatVector  or a SpatRaster")
    }
    if(!is.null(bcrNM)){
      bcrNM <- NULL
    }
  }

  # Valid bcrNM
  if (!is.null(bcrNM)){
    if(version == "v5"){
      base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_3978.shp", package = "BAMexploreR"))
    }else{
      base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_3978.shp", package = "BAMexploreR"))
    }
    if (!is.character(bcrNM)) {
      stop("bcrNM` must be a character vector representing valid BCR codes (e.g., 'can5', 'can80'). You provided an object of class: ", class(bcrNM)[1])
    }
    if (!all(bcrNM %in% base_bcr$bcr)) {
      stop("Invalid bcr value(s) provided: ", paste(setdiff(bcrNM, base_bcr$bcr), collapse = ", "))
    }

  }

  # Check destfile
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }

  allowed_years <- "2020"
  if(version == "v5"){
    if (!all(year %in% allowed_years)) {
      stop("Only 2020 predictions are currently available for public download. Predictions at five-year intervals from 1995 to 2015 are available by request from bamp@ualberta.ca.")
    }
  }

  valid_species <- bam_spp_list(version = version )
  if (!all(spList %in% valid_species)) {
    stop("Invalid species in spList: must be in bam_spp_list()")
  }

  spv <- bam_spp_list(version, "speciesCode")

  # Check if provided species list is in the available species codes. Display erroneous
  uspecies <- spList[!spList %in% spv]
  if (length(uspecies) > 0) {
    message("The following species aren't available for processing: ",
            paste(uspecies, collapse = ", "))
  }

  # Create valid species vector
  spList <- spList[spList %in% spv]

  if(version == "v5"){
    sp_filter <- .filter_species_by_bcr(birdlist, spList, bcrNM)
  } else{
    sp_filter <- spList
  }

  removed_species <- setdiff(spList, sp_filter)

  if(length(removed_species) > 0){
    message("Species out of range: ",
            paste(removed_species, collapse = ", "),
            ". No output is available for these species in the selected BCR: ", paste(bcrNM, collapse = ", "))
  }
  spList <- sp_filter

  if (length(spList) == 0) {
    stop("\n\nNo species remain to download for the selected BCR.")
  }

  outList <- list()

  # Perform batch download for species in the list
  for (s in spList) {
   if(version == "v4"){
     outspp <- .batch_download(species = s, year = NULL, version = version, crop_ext, bcrNM, destfile)
     outList <- append(outList, outspp)
   }else{
      for (y in year) {#v5
        outspp <- .batch_download(species = s, year = y, version = version, crop_ext, bcrNM, destfile)
        outList <- append(outList, outspp)
      }
   }
  }

  #Delete temp file
  temp_file <- tempfile(fileext = ".tif")
  on.exit({
    if (file.exists(temp_file)) file.remove(temp_file)
  })
  # Return the results as a list
  #setwd(cwd)
  return(outList)

}
