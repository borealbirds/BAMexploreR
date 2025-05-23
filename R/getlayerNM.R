##################################################################################
#' Download National Model species specific output map raster based on list of species
#'
#' @param spList character. A vector of species to be downloaded.

#' @param version character. Represents the version of the national model output to be downloaded.
#'
#' @param destfile character. Indicate output path where the downloaded file is saved.
#'
#' @param crop_ext SpatVector or SpatRaster used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: mapBCR("v4") or mapBCR("v5")
#'
#' @param year character; Specify the year for which the density map were generated. Only in v5.
#'
#' @param bcrNM character; vector representing the BCR according to model version. Default is "mosaic".

#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @importFrom dplyr pull
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname getlayerNM
#' @examples
#' bird <- getlayerNM("BAOR", "v4", tempfile())
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = tempdir())
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = ".", crop_ext = NULL)
#'
#'
getlayerNM <- function(spList, version, destfile, crop_ext = NULL,  year = NULL, bcrNM= "mosaic") {
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

  cwd <- getwd()
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }

  if (!missing(destfile)) {
    setwd(destfile)
    on.exit(setwd(cwd))
  }

  # Set url based on version
  url <- version.url$url[version.url$version == version]
  response <- httr::GET(url)
  content_text <- httr::content(response, "text")
  # Create list of available species
  if(version == "v4"){
    tiff_files <- regmatches(content_text, gregexpr('href="([^"]+\\.tif)"', content_text))
    tiff_files <- unlist(tiff_files)
    tiff_files <- gsub('href="|/"', '', tiff_files)
    spv <- tiff_files %>%
      stringr::str_sub(start = 6, end = 9)
  }else if(version == "v5"){
    subdirs <- regmatches(content_text, gregexpr('href="([^"]+/)"', content_text))
    subdirs <- unlist(subdirs)
    spv <- gsub('href="|/"', '', subdirs) %>%
      .[!(. %in% "/data")]
    flevel1 <- paste0(subdirs, "/")
  }

  # Check if provided species list is in the available species codes. Display erroneous
  uspecies <- spList[!spList %in% spv]
  if (length(uspecies) > 0) {
    tryCatch({
      warning(paste0("The following species aren't available for processing: ", paste(uspecies, collapse = ", ")))
    }, warning = function(w) {
      message(w$message)
    })
  }

  # Create valid species vector
  spList <- spList[spList %in% spv]

  outList <- list()
  # Batch download function
  batch_download <- function(species_code, version, crop_ext, bcrNM = "mosaic") {
    cat(paste0("Downloading data for ", species_code, " from version ", version), "\n")
    browser()
    if(inherits(crop_ext, "SpatVector") || inherits(crop_ext, "SpatRaster")){
      temp_file <- tempfile(fileext = ".tif")
      if(version == "v5"){
        region <- "mosaic"
        # Create a temporary file
        file_name <- paste0(species_code, "_", region, "_", year, ".tiff")
        file_url <- file.path(url, species_code, region, file_name)
      } else if(version == "v4"){
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        file_url <- file.path(url, file_name)
      }
      writeBin(content(GET(file_url), "raw"), temp_file)
      tiff_data <- rast(temp_file)

      if(!terra::same.crs(tiff_data, "EPSG:5072")) {
        tiff_data <- terra::project(tiff_data, "EPSG:5072")
      }

      if(inherits(crop_ext, "SpatVector")){
        tiff_data <- tiff_data %>%
          crop(project(crop_ext, tiff_data), snap="near", mask=TRUE)
        if(tools::file_ext(file_name) == "tif"){
          out_name <- sub("(\\.tif)$", "_clip\\1", file_name)
        }else{
          out_name <- sub("(\\.tiff)$", "_clip\\1", file_name)
        }
      }else if(inherits(crop_ext, "SpatRaster")){
        tiff_data <- tiff_data %>%
          crop(project(crop_ext, tiff_data, align_only = TRUE), snap="near", mask=TRUE)
        out_name <- sub("(\\.tif)$", "_clip\\1", file_name)
      }else{
        out_name <- paste0(species_code, "_", region, "_", year, ".tiff")
      }
      writeRaster(tiff_data, file.path(destfile, out_name), overwrite=TRUE)
      outList <- c(outList, setNames(list(tiff_data), species_code))

      # Delete the temporary file
      file.remove(temp_file)
      rm(tiff_data)
      return(outList)
    }else if(is.null(crop_ext)){
      if(version == "v4"){
        # Create a temporary file
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        file_url <- paste(url, file_name, sep= "/")
        temp_file <- tempfile(fileext = ".tif")

        writeBin(content(GET(file_url), "raw"), temp_file)
        tiff_data <- rast(temp_file)

        if(length(bcrNM)>1 && !"mosaic" %in% bcrNM){
          extent <- system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR")  %>%
            vect()
          extent <- extent[extent$subunit_ui %in% bcrNM, ]
          tiff_data <- tiff_data %>%
            crop(project(extent, tiff_data), snap="near", mask=TRUE)
        }

        # Construct output file name and save raster
        out_name <- file.path(destfile, paste0(tools::file_path_sans_ext(file_name), ".tif"))
        writeRaster(tiff_data, out_name, overwrite = TRUE)

        # Store result and clean up
        outList[[species_code]] <- tiff_data
        file.remove(temp_file)
        rm(tiff_data)
      }else if (version == "v5"){
        if(length(bcrNM)>1 && !"mosaic" %in% bcrNM){
          file_name <- paste0(species_code, "_mosaic_", year, ".tiff")
          file_url <- paste(url, species_code, "_mosaic_", file_name, sep= "/")
          writeBin(content(GET(file_url), "raw"), file.path(destfile, file_name))
          tiff_data <- rast(file.path(destfile, file_name))

          extent <- system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR")  %>%
            vect()
          extent <- extent[extent$subunit_ui %in% bcrNM, ]
          tiff_data <- tiff_data %>%
            crop(project(extent, tiff_data), snap="near", mask=TRUE)

        }else if(length(bcrNM)==1 && !"mosaic" %in% bcrNM){
          file_name <- paste0(species_code, "_", bcrNM, "_", year, ".tiff")
          file_url <- paste(url, species_code, "_", bcrNM,"_", file_name, sep= "/")
          writeBin(content(GET(file_url), "raw"), file.path(destfile, file_name))
          tiff_data <- rast(file.path(destfile, file_name))
        }else if("mosaic" %in% bcrNM){
          file_name <- paste0(species_code, "_mosaic_", year, ".tiff")
          file_url <- paste(url, species_code, "_mosaic_", file_name, sep= "/")
          writeBin(content(GET(file_url), "raw"), file.path(destfile, file_name))
          tiff_data <- rast(file.path(destfile, file_name))
        }

        outList <- c(outList, setNames(list(tiff_data), species_code))
        writeRaster(tiff_data, out_name, overwrite = TRUE)

        # Store result and clean up
        outList[[species_code]] <- tiff_data
        file.remove(temp_file)
        rm(tiff_data)
      }
      return(outList)
    }
  }

  # Perform batch download for species in the list
  for (s in spList) {
    outList <- batch_download(s, version, crop_ext, bcrNM)
  }

  # Return the results as a list
  return(outList)

}
