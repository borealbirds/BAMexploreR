##################################################################################
#' Retrieve list of BCR overlaid by the study area
#'
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' @return Vector of bcr overlaid by study area
#'
#' @import terra
#' @import dplyr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tmap tm_shape tm_polygons tm_layout
#' @importFrom sf st_as_sf
#' @docType methods
#' @author Melina Houle
#' @rdname mapBCR
#' @export
#' @examples
#' subUnit<- mapBCR()
mapBCR <- function(ext) {
  add_sf <- TRUE

  # Need output path
  if (missing(ext)) {
    ext <- vect(system.file("extdata", "BAM_BCR_NM.shp", package = "BAMexploreR"))
    add_sf <- FALSE
  }

  # Need SpatVector or SpatRaster and projection
  if(!class(ext)[1] %in% c("SpatVector", "SpatRaster")){
    stop("You need to provide a SpatRast or a SpatVect")
  }else{
    if (nchar(crs(ext)) == 0) {
      stop("CRS is missing or empty.")
    }
  }

  base_bcr <- vect(system.file("extdata", "BAM_BCR_NM.shp", package = "BAMexploreR"))

  label_sf <- st_as_sf(data.frame(
    X = base_bcr$X,
    Y = base_bcr$Y,
    label = base_bcr$subunit_ui,
    stringsAsFactors = FALSE
  ), coords = c("X", "Y"), crs = crs(base_bcr))

  # Ensure both SpatVect objects are in the same CRS
  if (crs(ext) != crs(base_bcr)) {
    ext <- project(ext, crs(base_bcr))
  }
  # Convert SpatVect objects to sf objects for use with tmap
  base_sf <- sf::st_as_sf(base_bcr)
  user_sf <- sf::st_as_sf(ext)

  # Find intersections
  if(add_sf){
    intersected <- st_intersects(base_sf, user_sf, sparse = FALSE)
    intersected_subUnits <- base_sf$subunit_ui[apply(intersected, 1, any)]
  }else{
    intersected_subUnits <-base_sf$subunit_ui
  }

  # Create the tmap
  # Generate a larger palette and subset it to get exactly 25 colors
  custom_palette <- RColorBrewer::brewer.pal(12, "Set3")  # Generate 12 colors from the Set3 palette
  custom_palette <- rep(custom_palette, length.out = 34)  # Repeat the palette to get 25 colors

  tmap <- tmap::tm_shape(base_sf) +
    tmap::tm_polygons(col = "subUnit", palette = custom_palette, style = "cat",  n = 34, border.col = "black", border.alpha = 0.5, legend.show = TRUE, id = "BCR subunit") +
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
