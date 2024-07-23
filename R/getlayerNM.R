##################################################################################
#' Download National Model species specific output map raster based on list of species
#'
#' @param spList A character string. A vector of species to be downloaded.

#' @param version A character string. Represents the version of the national model output to be downloaded.
#'
#' @param destfile A character string giving the path where the downloaded file is saved.
#'
#' @param type Logical. If \code{TRUE}, file is untar and/or unzip. Default is \code{FALSE}.
#'
#' @param crop Logical. If \code{TRUE}, crop raster layers to .
#'
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @importFrom googledrive drive_ls drive_get drive_download
#' @importFrom dplyr pull
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname getlayerNM
#' @examples
#' url <- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"
#' hashDownload(urls = url, destfile = tempdir(), checkhash = FALSE, cascade = FALSE)
#'
#'
getlayerNM <- function(spList, version, destfile, layer = "mean", crop = FALSE, y = NULL ) {
  library(googledrive)
  library(terra)
  library(dplyr)

  options(googledrive_quiet = TRUE)

  bcr_ls<- c("bcr9", "bcr10")


  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }

  # Need y if crop = TRUE
  if (crop){
    if(is.null(y)) {
      stop("You need to provide a SpatRast, SpatVect or a specific BCR unit in order to crop.")
    }else{
      if(!class(y)[1] %in% c("SpatVector", "SpatRaster", "character")){
        stop("You need to provide a SpatRast, SpatVect or a specific BCR unit in order to crop.")
      }
    }
  }

  # Need CRS
  if (!is.null(y)){
    if(class(y) == "SpatVect" | class(y) == "SpatRast") {
      if (nchar(crs(y)) == 0) {
        stop("CRS of cropping element is missing or empty.")
      }
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
  pid <- get(data(version.url))
  gd.list <- googledrive::drive_ls(pid$url[pid$version == version])
  # Create list of available species
  if(version == "v4"){
    spv <- gd.list %>%
      mutate(codesp = stringr::str_sub(name, start = 6, end = 9)) %>%
      pull(codesp)
  }else if(version == "v5"){
    spv <- gd.list %>%
      mutate(codesp = stringr::str_sub(name, start = 1, end = 4)) %>%
      pull(codesp)
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

  # Batch download function
  batch_download <- function(species_code, version, crop, y) {
    cat(paste0("Downloading data for ", species_code, " from version ", version, ifelse(crop, " with cropping.", " without cropping.")), "\n")

    # Crop and mask using y
    if(crop){
      browser()
      if(version == "v4"){
        ss <- gd.list %>%
          filter(grepl(species_code, name))
        # Create a temporary file
        temp_file <- tempfile(fileext = ".tif")
        # Download the file from Google Drive to the temporary location
        drive_download(as_id(ss$id), path = temp_file, overwrite = TRUE)
        tiff_data <- rast(temp_file)

        # reproject y based on tiff_data
        if(class(y)[1] == "SpatVector"){
          y <- project(y, tiff_data)
        }else if(class(y)[1] == "SpatRaster"){
          y <- project(y, tiff_data, align_only = TRUE)
        }else { #class(y)== "character"
          y <- system.file("extdata", "BAM_BCRNM_LAEA.shp", package = "BAMexploreR")  %>%
            vect() %>%
            dplyr::pull(subUnit==y) %>%
            project(tiff_data)
        }
        # Load the TIFF file, crop  and mask using y
        tiff_data <- tiff_data %>%
          crop(y, snap="near", mask=TRUE)
      }else{ # version v5
        ss <- gd.list %>%
          filter(grepl(species_code, name))
      }

      # Save
      writeRaster(tiff_data, file.path(destfile, ss$name), overwrite=TRUE)

      # Optionally delete the temporary file
      file.remove(temp_file)
      rm(tiff_data)
    }else{
      # Download the entire raster
      ss <- gd.list %>%
        filter(grepl(species_code, name))
      drive_download(as_id(ss$id), path =file.path(destfile, ss$name), overwrite=TRUE)
    }
  }

  # Perform batch download for species in the list
  for (s in spList) {
    batch_download(s, version, crop, y)
  }

  #return(sp)
}
