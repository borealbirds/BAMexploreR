##################################################################################
#' Map the boundaries of BCR subunits for a specified version.
#'
#' @param version Specifies the model release: \code{"v5"} for the current
#'   models or \code{"v4"} for the archived models.
#' @param ext A \code{SpatVector} or a \code{SpatRaster} used to define an area
#'   of interest.
#'
#' @param spList A \code{vector} of one species codes for which
#'   BCRs with available landbird density and habitat model rasters are
#'   mapped. If \code{NULL}, \code{spList} is ignored and all BCRs with available
#'   model outputs are mapped
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
#' @rdname bam_map_bcr
#' @export
#' @examples
#' subUnit<- bam_map_bcr("v5")
bam_map_bcr <- function(version, ext = NULL, spList = NULL) {
  tmap::tmap_mode("plot")

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }else if (version == "v4"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_3978.shp", package = "BAMexploreR"))
    ncat <-16
  }else if(version == "v5"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_3978.shp", package = "BAMexploreR")) |>
      terra::simplifyGeom(
        tolerance = 1000,
        preserveTopology = FALSE
      )
    base_bcr <- base_bcr[!base_bcr$bcr %in% c("Canada", "Lower48", "Alaska"),]
    ncat <-33
  }

  # Define bcr for requested species
  if (!is.null(spList)) {
    # Ensure only one species is passed
    if (length(spList) > 1) {
      stop("Only one species can be mapped at the time. ")
    }

    # Check that species codes exist in birdlist
    invalid_species <- setdiff(spList, colnames(birdlist))

    if (length(invalid_species) > 0) {
      stop(
        "The following species codes are not available: ",
        paste(invalid_species, collapse = ", ")
      )
    }

    # Keep BCRs where all requested species have model outputs
    available_bcr <- birdlist |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(spList),
          ~ .x
        )
      ) |>
      dplyr::pull(bcr)

    # Restrict BCR layer to available species BCRs
    selected_bcr <- base_bcr[base_bcr$bcr %in% available_bcr,]
  }else{
    selected_bcr <- base_bcr
  }

  # Convert SpatVect objects to sf objects for use with tmap
  selected_sf <- sf::st_as_sf(selected_bcr)

  # Need SpatVector or SpatRaster and projection
  if(!is.null(ext)){
    if(!inherits(ext, "SpatVector") && !inherits(ext, "SpatRaster")){
      stop("You need to provide a SpatRast or a SpatVect")
    }else if (nchar(terra::crs(ext)) == 0){
      stop("CRS is missing or empty.")
    } else {
      if (length(terra::intersect(base_bcr, ext)) == 0) {
        warning("The provided extent does not intersect with any BCR sub-units.")
      }else{
        # Ensure both SpatVect objects are in the same CRS
        if (terra::crs(ext) != "EPSG:3978") {
          extent <- terra::project(ext, "EPSG:3978")
        } else {
          extent <- ext
        }
        user_sf <- sf::st_as_sf(extent)
      }
    }

    intersected <- sf::st_intersects(
      selected_sf,
      user_sf,
      sparse = FALSE
    )

    intersected_subUnits <- selected_sf[
      apply(intersected, 1, any)
    ]
  }else{
    intersected_subUnits <- selected_sf
  }

  # Create the tmap
  # Generate a larger palette and subset it to get exactly 25 colors
  custom_palette <- RColorBrewer::brewer.pal(12, "Set3")  # Generate 12 colors from the Set3 palette
  custom_palette <- rep(custom_palette, length.out = ncat)  # Repeat the palette to get 25 colors

  tmap::tm_options(component.autoscale = FALSE)
  tmap <- tmap::tm_shape(base_bcr) +
    tmap::tm_polygons(
      fill = NA,
      col = "grey40",
      lwd = 1
  ) +
  tmap::tm_shape(intersected_subUnits) +
      tmap::tm_polygons(fill = "bcr",
                        fill.scale = tm_scale_categorical(values = custom_palette),
                        col = "black", col_alpha = 0.5,
                        fill.legend = NULL,
                        id = "bcr") +
      tmap::tm_add_legend(type = "polygons",  # Updated from "fill"
                          labels = unique(intersected_subUnits$bcr),
                          title = "BCR",
                          fill = custom_palette[seq_along(unique(intersected_subUnits$bcr))]) +  # Use `fill` instead of `col`

      tmap::tm_layout(legend.outside = TRUE, legend.is.portrait = FALSE, legend.stack = "horizontal")

  if(!missing(ext)){
    tmap <- tmap +
      tmap::tm_shape(user_sf) +
      tmap::tm_polygons(fill = NA, fill_alpha = 0, col = "black", col_alpha = 1, lwd = 3, fill.legend = NULL) +
      tmap::tm_add_legend(type = "polygons", labels = "User AOI", fill = NA, col = "black") + # Add legend item for user_sf
      tmap::tm_layout(legend.outside = TRUE)
  }

  # Return the results as a list
  return(tmap)
}
