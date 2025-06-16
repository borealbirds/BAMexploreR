##################################################################################
#' Retrieve list of BCR overlaid by the study area
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param ext SpatVector, SpatExtent, or SpatRaster used to define the extent for the cropping.
#'
#' @return Map illustrating the BCR and overlap extent if provided.
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
mapBCR <- function(version, ext = NULL) {
  tmap::tmap_mode("plot")

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }

  # Need SpatVector or SpatRaster and projection
  if (missing(ext)) {
    if(version == "v4"){
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
    }else {
      ext <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
    }
  }else {
    if(!inherits(ext, "SpatVector") && !inherits(ext, "SpatRaster")){
      stop("You need to provide a SpatRast or a SpatVect")
    }else{
      if (nchar(terra::crs(ext)) == 0) {
        stop("CRS is missing or empty.")
      }
    }
  }

  if(version == "v4"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_5072.shp", package = "BAMexploreR"))
    ncat <-16
  }else if(version == "v5"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_5072.shp", package = "BAMexploreR"))
    ncat <-33
  }else{
    stop("Model version doesn't exist.")
  }

  if (length(terra::intersect(base_bcr, ext)) == 0) {
    warning("The provided extent does not intersect with any BCR sub-units.")
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

  # Create the tmap
  # Generate a larger palette and subset it to get exactly 25 colors
  custom_palette <- RColorBrewer::brewer.pal(12, "Set3")  # Generate 12 colors from the Set3 palette
  custom_palette <- rep(custom_palette, length.out = ncat)  # Repeat the palette to get 25 colors

  tmap <- tmap::tm_shape(base_sf) +
      tmap::tm_polygons(fill = "subunit_ui",
                        fill.scale = tm_scale_categorical(values = custom_palette),
                        col = "black", col_alpha = 0.5,
                        fill.legend = NULL,
                        id = "subunit_ui") +
      tmap::tm_add_legend(type = "polygons",  # Updated from "fill"
                          labels = unique(base_sf$subunit_ui),
                          title = "BCR subunit",
                          fill = custom_palette[seq_along(unique(base_sf$subunit_ui))]) +  # Use `fill` instead of `col`

      tmap::tm_layout(legend.outside = TRUE, legend.is.portrait = FALSE, legend.stack = "horizontal")

  if(!missing(ext)){
    tmap <- tmap +
      tmap::tm_shape(user_sf) +
      tmap::tm_polygons(fill = NA, fill_alpha = 0, col = "red", col_alpha = 1, lwd = 2, fill.legend = NULL) +
      tmap::tm_add_legend(type = "polygons", labels = "User AOI", fill = NA, col = "red") + # Add legend item for user_sf
      tmap::tm_layout(legend.outside = TRUE, legend.stack = "vertical")
  }

  # Return the results as a list
  return(tmap)
}
