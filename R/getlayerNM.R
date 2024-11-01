##################################################################################
#' Download National Model species specific output map raster based on list of species
#'
#' @param spList character. A vector of species to be downloaded.

#' @param version character. Represents the version of the national model output to be downloaded.
#'
#' @param destfile character. Indicate output path where the downloaded file is saved.
#'
#' @param layer character; Name of the output layers of interest, either "mean", "sd" or "overextrapolated".
#'
#' @param crop logical. If \code{TRUE}, crop raster layers using \code{ext/}.
#'
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: mapBCR("v4") or mapBCR("v5")
#'
#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @importFrom googledrive drive_ls drive_get drive_download drive_auth
#' @importFrom dplyr pull
#' @importFrom stringr str_sub
#' @importFrom terra vect rast project crop values
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname getlayerNM
#' @examples
#' bird <- getlayerNM("BAOR", "v4", "mean",  tempfile())
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = tempdir(), layer = "mean", crop = FALSE)
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = ".", "mean", crop = FALSE, ext = NULL)
#'
#'
getlayerNM <- function(spList, version, destfile, layer = "mean", crop = FALSE, ext = NULL) {
  # List where all bcrunit are listed
  bcr_v5 <- c("bcr9", "bcr10")

  # Valid Model versions
  if (!version %in% c("v4", "v5")) {
    stop("Model version doesn't exist.")
  }

  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }

  # Need ext if crop = TRUE
  if (crop){
    if(is.null(ext)) {
      stop("You need to provide a SpatRast, SpatVect or a specific BCR unit in order to crop.")
    }else{
      if(!class(ext)[1] %in% c("SpatVector", "SpatRaster", "character")){
        stop("You need to provide a SpatRast, SpatVect or a specific BCR unit in order to crop.")
      }
    }
  }

  # Need CRS
  if (!is.null(ext)){
    if(class(ext) == "SpatVect" | class(ext) == "SpatRast") {
      if (nchar(crs(ext)) == 0) {
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
  pid <- get("version.url", envir = asNamespace("BAMexploreR"))
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

  outList <- list()
  # Batch download function
  batch_download <- function(species_code, version, crop, ext) {
    cat(paste0("Downloading data for ", species_code, " from version ", version, ifelse(crop, " with cropping.", " without cropping.")), "\n")
    # Crop and mask using y
    if(crop){
      if(version == "v4"){
        ss <- gd.list %>%
          filter(grepl(species_code, name))
      }else if(version == "v5"){
        if(class(ext) == "character"){
          if(ext %in% bcr_v5){
            ss <- gd.list %>%
              filter(grepl(species_code, name), grepl(ext, name))
          }else{
            stop("you need to specify a valid subunit. Please run mapBCR to identify the one of interest")
          }
        }else{
          ss <- gd.list %>%
            filter(grepl(species_code, name), grepl("national", name))
        }
      }
      # Create a temporary file
      temp_file <- tempfile(fileext = ".tif")
      # Download the file from Google Drive to the temporary location
      drive_download(as_id(ss$id), path = temp_file, overwrite = TRUE)
      tiff_data <- rast(temp_file)


      # reproject ext based on tiff_data
      if(class(ext)[1] == "SpatVector"){
        y <- project(ext, tiff_data)
        tiff_data <- tiff_data %>%
          crop(y, snap="near", mask=TRUE)
      }else if(class(ext)[1] == "SpatRaster"){
        y <- project(ext, tiff_data, align_only = TRUE)
        tiff_data <- tiff_data %>%
          crop(y, snap="near", mask=TRUE)
      }else {#class(y)== "character"
        if(version == "v4"){
          y <- system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR")  %>%
            vect() %>%
            dplyr::pull(subUnit==y)
        }else{
          return(tiff_data)
        }
      }

      # V5: select the right layer (mean, cv or extrapolation)
      if(version == "v5"){
        tiff_data <- tiff_data[[layer]]
      }

      # Save
      writeRaster(tiff_data, file.path(destfile, ss$name), overwrite=TRUE)
      outList <- c(outList, setNames(list(tiff_data), species_code))

      # Delete the temporary file
      file.remove(temp_file)
      rm(tiff_data)
    }else{
      # Download the entire raster
      ss <- gd.list %>%
        filter(grepl(species_code, name))
      drive_download(as_id(ss$id), path =file.path(destfile, ss$name), overwrite=TRUE)
      tiff_data <- rast(file.path(destfile, ss$name))
      outList <- c(outList, setNames(list(tiff_data), species_code))
    }
    return(outList)
  }

  # Perform batch download for species in the list
  for (s in spList) {
    outList <- batch_download(s, version, crop, ext)
  }

  # Return the results as a list
  return(outList)

}
