##################################################################################
#' Retrieve list of BCR overlaid by the study area
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' @return Vector of bcr that overlay the study area.
#'
#' @importFrom terra vect crs project
#' @importFrom sf st_as_sf st_intersects
#' @docType methods
#' @author Melina Houle
#' @rdname getBCR
#' @export
#' @examples
#' subUnit<- mapBCR("v5")
getBCR <- function(version, ext) {
  # Need output path
  if (missing(version)) {
    stop("You must specified either v4 or v5")
  }

  # Need output path
  if (missing(ext)) {
    if(version == "v4"){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
    }else if (version == "v5"){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
    }else{
      stop("The version is not recognised by the function. BAM National Models are only available for v4 and v5.")
    }
  }

  # Need SpatVector or SpatRaster and projection
  if(!inherits(ext, "SpatVector") && !inherits(ext, "SpatRaster")){
    stop("You need to provide a SpatRast or a SpatVect")
  }else{
    if (nchar(terra::crs(ext)) == 0) {
      stop("CRS is missing or empty.")
    }
  }

  if(version == "v4"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
  }else if(version == "v5"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
  }else{
    stop("Model version doesn't exist.")
  }

  # Convert SpatVect objects to sf objects for use with tmap
  base_sf <- sf::st_as_sf(base_bcr)

  # Find intersections
  if(!missing(ext)){
    # Ensure both SpatVect objects are in the same CRS
    if (terra::crs(ext) != terra::crs(base_bcr)) {
      ext <- terra::project(ext, terra::crs(base_bcr))
    }
    user_sf <- sf::st_as_sf(ext)
    intersected <- sf::st_intersects(base_sf, user_sf, sparse = FALSE)
    intersected_subUnits <- base_sf$subunit_ui[apply(intersected, 1, any)]
  }else{
    intersected_subUnits <-base_sf$subunit_ui
  }

  # Return the results as a list
  return(intersected_subUnits)
}
