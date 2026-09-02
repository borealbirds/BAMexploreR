##################################################################################
#' Retrieve the list of BCR overlaid by the study area
#'
#' @param version Specifies the model release: \code{"v5"} for the current
#'   models or \code{"v4"} for the archived models.
#' @param ext A \code{SpatVector} or \code{SpatRaster} used to define the extent
#'  for the cropping. If \code{NULL}, \code{ext} is ignored and all
#'  BCRs with available model outputs are returned..
#' @param spList A \code{vector} of one or more species codes for which
#'   BCRs with available landbird density and habitat model rasters should
#'   be returned. If multiple species are specified, only BCRs with
#'   available model outputs for \emph{all} specified species are returned.
#'   If \code{NULL}, \code{spList} is ignored and all BCRs with available
#'   model outputs are returned.
#' @param common_bcr Logical. If \code{TRUE} and multiple species are
#'   specified in \code{spList}, only BCRs with available model outputs for
#'   all specified species are returned. If \code{FALSE}, BCRs with
#'   available model outputs for any of the specified species are returned.
#'   Ignored if \code{spList} is \code{NULL}.
#'
#' @return Vector of bcr that overlay the study area.
#'
#' @importFrom terra vect crs project
#' @importFrom sf st_as_sf st_intersects
#' @docType methods
#' @author Melina Houle
#' @rdname bam_get_bcr
#' @export
#' @examples
#' subUnit<- bam_get_bcr("v5")
bam_get_bcr <- function(version, ext = NULL, spList = NULL, common_bcr = TRUE) {

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }

  # Set base_bcr
  if(version == "v4"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv4_3978.shp", package = "BAMexploreR"))
  }else if (version == "v5"){
    base_bcr <- terra::vect(system.file("extdata", "BAM_BCRNMv5_3978.shp", package = "BAMexploreR")) |>
      terra::simplifyGeom(
        tolerance = 1000,
        preserveTopology = FALSE
      )
  }else{
    stop("The version is not recognised by the function. BAM Landbird Density & Habitat models are only available for v4 and v5.")
  }

  # Define extent
  if (is.null(ext)) {
    user_sf <- sf::st_as_sf(base_bcr)
  }else{
    # Check extent class
    if (!inherits(ext, "SpatVector") && !inherits(ext, "SpatRaster")) {
      stop("You need to provide a SpatRast or a SpatVect.")
    }
    # Check CRS
    if (nchar(terra::crs(ext)) == 0) {
      stop("CRS is missing or empty.")
    }
    # Ensure both objects have the same CRS
    if (terra::crs(ext) != terra::crs(base_bcr)) {
      ext <- terra::project(ext, terra::crs(base_bcr))
    }
    user_sf <- sf::st_as_sf(ext)
  }

  # Find BCRs with model outputs for requested species
  if (!is.null(spList)) {

    # Check that species codes exist in birdlist
    invalid_species <- setdiff(spList, colnames(birdlist))

    if (length(invalid_species) > 0) {
      stop(
        "The following species codes are not available: ",
        paste(invalid_species, collapse = ", ")
      )
    }

    if(isTRUE(common_bcr)){
      available_bcr <- birdlist |>
         dplyr::filter(
           dplyr::if_all(
             dplyr::all_of(spList),
             ~ .x
           )
         ) |>
         dplyr::pull(bcr)

       # Restrict BCR layer to available species BCRs
       base_bcr <- base_bcr[base_bcr$bcr %in% available_bcr,]

    }else{
      # Get BCRs independently for each species
      bcr_by_species <- lapply(spList, function(sp) {

        birdlist |>
          dplyr::filter(.data[[sp]]) |>
          dplyr::pull(bcr) |>
          unique()

      })

      # Name the list with species codes
      names(bcr_by_species) <- spList

      # Restrict each species to BCRs intersecting the extent
      if (!is.null(ext)) {

        intersected <- sf::st_intersects(
          sf::st_as_sf(base_bcr),
          user_sf,
          sparse = FALSE
        )

        intersected_bcr <- base_bcr$bcr[
          apply(intersected, 1, any)
        ]

        bcr_by_species <- lapply(
          bcr_by_species,
          function(x) x[x %in% intersected_bcr]
        )
      }

      return(bcr_by_species)
    }
  }

  # Convert to sf for intersection
  base_sf <- sf::st_as_sf(base_bcr)

  # Find intersections
  if(!is.null(ext)){
    intersected <- sf::st_intersects(
      base_sf,
      user_sf,
      sparse = FALSE
    )

    intersected_subUnits <- base_sf$bcr[
      apply(intersected, 1, any)
    ]
  }else{
    intersected_subUnits <- base_sf$bcr
  }

  return(intersected_subUnits)
}

