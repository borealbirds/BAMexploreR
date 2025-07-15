##################################################################################
#' Download BAM species specific Landbird Models density maps based on list of species
#'
#' @param spList A \code{vector} of species to be downloaded.

#' @param version A \code{character} specifying which version of the BAM Landbird Models to use. Valid options are "v4" or "v5".
#'
#' @param destfile A \code{character} indicating output path where the downloaded file is saved.
#'
#' @param crop_ext A \code{SpatVector} or A \code{SpatRaster} used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: \code{bam_map_bcr("v4")} or \code{bam_map_bcr("v5")}
#'
#' @param year A \code{character} specifying the year for which the density map were generated. Only in v5.
#' Valid options are "1985", "1990", "1995", "2000", "2005", "2010", "2015" and "2020". Default value is "2020".
#'
#' @param bcrNM A \code{vector} representing the BCR subunit name according to model version. Default is "mosaic".
#'
#' @return A list of \code{SpatRaster} objects. In addition to returning these objects,
#' the function also downloads raster files to the directory specified by \code{destfile},
#' as a side-effect.
#'
#' @examples
#' bird <- bam_get_layer("TEWA", "v4", tempfile())
#'
#' bird <- bam_get_layer("TEWA", "v4", destfile = tempdir())
#'
#' @author Melina Houle
#' @docType methods
#' @rdname bam_get_layer
#' @export
#'
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
#' @importFrom httr GET content
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values crs writeRaster same.crs expanse
#' @importFrom stats setNames
#'
bam_get_layer <- function(spList, version, destfile, crop_ext = NULL,  year = NULL, bcrNM= "mosaic") {
  # Valid Model versions
  if (!version %in% c("v4", "v5")) {
    stop("Model version doesn't exist.")
  }

  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }

  if (is.null(year)){
    if(version == "v5"){
      year <- c("2020")
    }
  }

  if (!is.null(bcrNM)){
    if (!is.character(bcrNM)) {
      stop("bcrNM` must be a character vector representing valid BCR codes (e.g., 'can5', 'can80'). You provided an object of class: ", class(bcrNM)[1])
    }
    if(version == "v5"){
      valid_bcrs <- c("mosaic", "can3", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                      "can40", "can41", "can42", "can60", "can61", "can70", "can71", "can72", "can80",
                      "can81", "can82", "usa2", "usa5", "usa9", "usa10", "usa11", "usa12", "usa13",
                      "usa14", "usa23", "usa28", "usa30", "usa40", "usa43", "usa41423")
      if (!all(bcrNM %in% valid_bcrs)) {
        stop("Invalid bcr value(s) provided: ", paste(setdiff(bcrNM, valid_bcrs), collapse = ", "))
      }
    }else{
      valid_bcrs <- c("mosaic", "can4", "can5", "can9", "can10", "can11", "can12", "can13", "can14",
                      "can60", "can61", "can70", "can71", "can80", "can81", "can82", "can83")
      if (!all(bcrNM %in% valid_bcrs)) {
        stop("Invalid bcr value(s) provided: ", paste(setdiff(bcrNM, valid_bcrs), collapse = ", "))
      }
    }
  }

  # Need CRS
  if (!is.null(crop_ext)){
    if(inherits(crop_ext, "SpatVector") || inherits(crop_ext, "SpatRaster") ) {
      if (nchar(crs(crop_ext)) == 0) {
        stop("CRS of crop_ext is missing or empty.")
      }else{
        if (crs(crop_ext, describe = TRUE)$code != 5072) {
          crop_ext <- terra::project(crop_ext, "EPSG:5072")
        }
      }
    }else{
      stop("crop_ext need to be a SpatVector  or a SpatRaster")
    }
  }

  #cwd <- getwd()
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }

  # Check crop_ext area
  if(!is.null(crop_ext)){
    crop_area <- expanse(crop_ext, unit="km")
    if(crop_area < 100){
      warning(sprintf("The BAM density models are predicted to a resolution of 1 km2. Your area of interest is only %.2f km2. Please consider whether these models are appropriate for your application.", crop_area))
    }
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

  outList <- list()

  batch_download <- function(species_code, version, year = NULL, crop_ext, bcrNM = "mosaic") {
    message("Downloading data for ", species_code, " from version ", version)

    # get file name and URL
    get_file_info <- function() {
      if (version == "v4") {
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        file_url <- file.path(url, file_name)
      } else if (version == "v5") {
        region <- ifelse(length(bcrNM) == 1, bcrNM, "mosaic")
        file_name <- paste0(species_code, "_", region, "_", year, ".tiff")
        file_url <- file.path(url, species_code, region, file_name)
      }
      list(name = file_name, url = file_url)
    }

    # download raster to a file
    download_raster <- function(file_url, to_temp = TRUE) {
      target_file <- if (to_temp) tempfile(fileext = ".tif") else file.path(destfile, basename(file_url))
      writeBin(content(GET(file_url), "raw"), target_file)
      rast(target_file)
    }

    #crop raster to extent
    crop_raster <- function(r, ext) {
      r_proj <- terra::project(ext, r)
      terra::crop(r, r_proj, snap = "near", mask = TRUE)
    }

    # Get file info
    url <- version.url$url[version.url$version == version]
    file_info <- get_file_info()
    file_name <- file_info$name
    file_url  <- file_info$url

    # create output name
    out_name <- paste0(tools::file_path_sans_ext(file_name), ".tif")

    # Main raster loading
    if (inherits(crop_ext, c("SpatVector", "SpatRaster"))) {
      tiff_data <- download_raster(file_url, to_temp = TRUE)

      tiff_data <- if (inherits(crop_ext, "SpatVector")) {
        crop_raster(tiff_data, crop_ext)
      } else {
        crop_raster(tiff_data, project(crop_ext, tiff_data, align_only = TRUE))
      }

      out_name <- sub("\\.tiff?$", "_clip.tif", file_name)

    } else if ("mosaic" %in% bcrNM) {
      tiff_data <- download_raster(file_url, to_temp = (version == "v4"))

    } else if(length(bcrNM)>1 || (length(bcrNM) == 1 && version == "v4")){
      tiff_data <- download_raster(file_url, to_temp = (version == "v4"))

      extent <- system.file(
        "extdata",
        ifelse(version == "v4", "BAM_BCRNMv4_5072.shp", "BAM_BCRNMv5_5072.shp"),
        package = "BAMexploreR"
      ) %>% vect()
      extent <- extent[extent$subunit_ui %in% bcrNM, ]
      tiff_data <- crop_raster(tiff_data, extent)

      if (version == "v4"){
        out_name <- paste0(species_code, "-CAN-Mean_BCRclip.tif")
      }else{
        out_name <- paste0(species_code, "-mosaic-", year, "-BCRclip.tif")
      }
    }

    if (!terra::same.crs(tiff_data, "EPSG:5072"))
      tiff_data <- terra::project(tiff_data, "EPSG:5072")

    terra::writeRaster(tiff_data, file.path(destfile, out_name), overwrite = TRUE)
    return(setNames(list(tiff_data), species_code))
  }

  # Perform batch download for species in the list
  for (s in spList) {
   if(version == "v4"){
     outspp <- batch_download(species = s, year = NULL, version = version, crop_ext, bcrNM)
     outList <- append(outList, outspp)
   }else{
      for (y in year) {#v5
        outspp <- batch_download(species = s, year = y, version = version, crop_ext, bcrNM)
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
