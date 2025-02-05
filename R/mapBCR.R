##################################################################################
#' Retrieve list of BCR overlaid by the study area
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' @return List containing vector of bcr that overlay the study area and a map illustrating the overlap.
#'
#' @import tmap
#' @importFrom terra vect crs project
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tmap tm_shape tm_polygons tm_layout tm_text tm_add_legend tmap_mode
#' @importFrom sf st_as_sf st_intersects
#' @docType methods
#' @author Melina Houle
#' @rdname mapBCR
#' @export
#' @examples
#' subUnit<- mapBCR("v5")
mapBCR <- function(version, ext) {
  add_sf <- TRUE
  tmap::tmap_mode("plot")
  # Need output path
  if (missing(version)) {
    stop("You must specified either v4 or v5")
  }

  # Need output path
  if (missing(ext)) {
    if(version == "v4" || version == "v4_demo" ){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
      add_sf <- FALSE
    }else if (version == "v5" || version == "v5_demo" ){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv5_LAEA.shp", package = "BAMexploreR"))
      add_sf <- FALSE
    }else{
      stop("The version is not recognised by the function. BAM National Models are only available for v4 and v5.")
    }
  }

  # Need SpatVector or SpatRaster and projection
  if(!class(ext)[1] %in% c("SpatVector", "SpatRaster")){
    stop("You need to provide a SpatRast or a SpatVect")
  }else{
    if (nchar(terra::crs(ext)) == 0) {
      stop("CRS is missing or empty.")
    }
  }

  if(version == "v4" || version == "v4_demo"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_LAEA.shp", package = "BAMexploreR"))
    ncat <-16
  }else if(version == "v5" || version == "v5_demo"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_LAEA.shp", package = "BAMexploreR"))
    ncat <-32
  }else{
    stop("Model version doesn't exist.")
  }

  label_sf <- sf::st_as_sf(data.frame(
    X = base_bcr$X,
    Y = base_bcr$Y,
    label = base_bcr$subunit_ui,
    stringsAsFactors = FALSE
  ), coords = c("X", "Y"), crs = terra::crs(base_bcr))

  # Ensure both SpatVect objects are in the same CRS
  if (terra::crs(ext) != terra::crs(base_bcr)) {
    ext <- terra::project(ext, terra::crs(base_bcr))
  }
  # Convert SpatVect objects to sf objects for use with tmap
  base_sf <- sf::st_as_sf(base_bcr)
  user_sf <- sf::st_as_sf(ext)

  # Find intersections
  if(add_sf){
    intersected <- sf::st_intersects(base_sf, user_sf, sparse = FALSE)
    intersected_subUnits <- base_sf$subunit_ui[apply(intersected, 1, any)]
  }else{
    intersected_subUnits <-base_sf$subunit_ui
  }

  # Create the tmap
  # Generate a larger palette and subset it to get exactly 25 colors
  custom_palette <- RColorBrewer::brewer.pal(12, "Set3")  # Generate 12 colors from the Set3 palette
  custom_palette <- rep(custom_palette, length.out = ncat)  # Repeat the palette to get 25 colors

  tmap <- tmap::tm_shape(base_sf) +
      tmap::tm_polygons(col = "subunit_ui", palette = custom_palette, style = "cat",  n = ncat, border.col = "black", border.alpha = 0.5, legend.show = TRUE, id = "subunit_ui") +
      tmap::tm_shape(label_sf) + # Add the label_sf shape here
      tmap::tm_text(text = "label", size = 0.7, col = "black", shadow = TRUE) +
      tmap::tm_layout(legend.position = c("left", "bottom"))

  if(add_sf){
    tmap <- tmap +
      tmap::tm_shape(user_sf) +
      tmap::tm_polygons(col = NA, alpha = 0, border.col = "black", border.alpha = 1, lwd = 2, legend.show = TRUE) +
      tmap::tm_add_legend(type = "fill", labels = "User AOI", col = NA, border.col = "black") + # Add legend item for user_sf
      tmap::tm_layout(legend.position = c("left", "bottom"))
  }

  # Return the results as a list
  return(list(subUnits = intersected_subUnits, map = tmap))
}
