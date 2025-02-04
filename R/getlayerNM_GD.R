##################################################################################
#' Download National Model species specific output map raster based on list of species
#'
#' @param spList character. A vector of species to be downloaded.

#' @param version character. Represents the version of the national model output to be downloaded.
#'
#' @param destfile character. Indicate output path where the downloaded file is saved.
#'
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#' Or downloading valid BCR polygons from list, type: mapBCR("v4") or mapBCR("v5")
#'
#' @param year character; Specify the year for which the density map were generated. Only in v5.
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
#' @rdname getlayerNM_GD
#' @examples
#' bird <- getlayerNM("BAOR", "v4", "mean",  tempfile())
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = tempdir(), layer = "mean", crop = FALSE)
#'
#' bird <- getlayerNM("BAOR", "v4", destfile = ".", "mean", crop = FALSE, ext = NULL)
#'
#'
getlayerNM_GD <- function(spList, version, destfile, ext = NULL,  year = NULL) {
  # Valid Model versions
  if (!version %in% c("v4", "v4_demo", "v5", "v5_demo")) {
    stop("Model version doesn't exist.")
  }

  # Need output path
  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded rasters.")
  }

  if (is.null(year)){
    if(version == "v5" || version == "v5_demo"){
      year <- c("2020")
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
  if(version == "v4" || version == "v4_demo"){
    spv <- gd.list %>%
      mutate(codesp = stringr::str_sub(name, start = 6, end = 9)) %>%
      pull(codesp)
  }else if(version == "v5" || version == "v5_demo"){
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
  batch_download <- function(species_code, version, ext) {
    cat(paste0("Downloading data for ", species_code, " from version ", version), "\n")
    if(version=="v4_demo"){
      version<- "v4"
    }else if(version=="v5_demo"){
      version<- "v5"
    }

    if(class(ext)[1]=="SpatVector" || class(ext)[1]=="SpatRaster"){
      temp_file <- tempfile(fileext = ".tif")
      if(version == "v5"){
        region <- "mosaic"
        # Create a temporary file
        file_name <- paste0(species_code, "_", region, "_", year, ".tiff")
        full_path <- file.path("NM_output_demo", version, species_code, region, file_name)
      } else if(version == "v4"){
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        full_path <- file.path("NM_output_demo", version, file_name)
      }
      file_info <- drive_get(full_path, shared_drive= "BAM_Core")
      drive_download(as_id(file_info$id), path =temp_file, overwrite=TRUE)
      tiff_data <- rast(temp_file)
      if(class(ext)[1] == "SpatVector"){
        y <- project(ext, tiff_data)
        tiff_data <- tiff_data %>%
          crop(y, snap="near", mask=TRUE)
      }else if(class(ext)[1] == "SpatRaster"){
         y <- project(ext, tiff_data, align_only = TRUE)
        tiff_data <- tiff_data %>%
          crop(y, snap="near", mask=TRUE)
      }
      # Save
      writeRaster(tiff_data, file.path(destfile, file_info$name), overwrite=TRUE)
      outList <- c(outList, setNames(list(tiff_data), species_code))
      # Delete the temporary file
      file.remove(temp_file)
      rm(tiff_data)
      return(outList)
    }else if(class(ext) == "character"){
      if(version == "v4"){
        ext <- system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR")  %>%
          vect() %>%
          dplyr::pull(subUnit==ext)
        # Create a temporary file
        temp_file <- tempfile(fileext = ".tif")
        file_name <- paste0("pred-", species_code, "-CAN-Mean.tif")
        full_path <- file.path("NM_output_demo", version, file_name)
        file_info <- drive_get(full_path, shared_drive= "BAM_Core")
        drive_download(as_id(file_info$id), path =temp_file, overwrite=TRUE)
        tiff_data <- rast(temp_file)
        y <- project(ext, tiff_data)
        tiff_data <- tiff_data %>%
          crop(y, snap="near", mask=TRUE)
        writeRaster(tiff_data, file.path(destfile, file_info$name), overwrite=TRUE)
        outList <- c(outList, setNames(list(tiff_data), species_code))
        # Delete the temporary file
        file.remove(temp_file)
        rm(tiff_data)
      }else if (version == "v5"){
        file_name <- paste0(species_code, "_", ext, "_", year, ".tiff")
        full_path <- file.path("NM_output_demo", version, species_code, ext, file_name)
        file_info <- drive_get(full_path, shared_drive= "BAM_Core")
        drive_download(as_id(file_info$id), path =file.path(destfile, file_info$name), overwrite=TRUE)
        tiff_data <- rast(file.path(destfile, file_info$name))
        outList <- c(outList, setNames(list(tiff_data), species_code))
      }
    }
    return(outList)
  }

  # Perform batch download for species in the list
  for (s in spList) {
    outList <- batch_download(s, version, ext)
  }

  # Return the results as a list
  return(outList)

}
