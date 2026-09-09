if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".env","predictor_class", "mean_rel_inf", "sd_rel_inf", "species", "spp", "sum_inf", "sum_all_groups",
                           "pooled_sd", "percent_inf", "sym", "sd_percent_inf", "guild_opt", "speciesCode", "commonName",
                           "scientificName", "sum_influence", "sum_group1", "prop", "density", "spp_tbl", "bam_predictor_response_v5",
                           # predictor-importance aggregation (per-model shares and uncertainty)
                           "bcr", "predictor", "boot", "n_boots", "n_boot", ".scale", "share", "approx_var",
                           "unit_var", "n_units", "sum_share", "sum_share_sq", "sum_unit_var", "mean_share",
                           "among_var", "within_var", "se_share", "sd_among", "sd_among_inf",
                           "bam_predictor_importance_v4", "bam_predictor_importance_v5",
                           "bam_predictor_boot_v5", "rel.inf"))
}

# Session-scoped record of which advisory warnings have already been shown, so
# that repeated calls in a script or vignette do not bury the console in
# identical messages.
.bam_warn_state <- new.env(parent = emptyenv())

# Session-scoped memo for derived tables. `bam_predictor_boot_v5` is the stored
# source of truth and everything else is rolled up from it, so a small cache
# keeps repeated calls (a vignette, a loop over species) from redoing the same
# aggregation over ~1.7 million rows.
.bam_cache <- new.env(parent = emptyenv())

#' Fetch a shipped dataset by name
#'
#' LazyData puts shipped datasets in the package namespace on a normal install,
#' but \code{devtools::load_all()} and a plain \code{data()} load can place them
#' elsewhere, so search both and return \code{NULL} rather than erroring when the
#' dataset is not shipped at all (as for v4 bootstrap data).
#'
#' @param nm A \code{character} dataset name.
#' @return The dataset, or \code{NULL}.
#' @noRd
.get_dataset <- function(nm) {
  out <- tryCatch(
    get(nm, envir = asNamespace("BAMexploreR")),
    error = function(e) tryCatch(get(nm), error = function(e) NULL)
  )
  if (is.null(out) || !is.data.frame(out)) NULL else out
}

#' Emit a warning at most once per session
#'
#' @param id A \code{character} key identifying the warning.
#' @param ... Passed to \code{warning()}.
#' @noRd
.warn_once <- function(id, ...) {
  if (isTRUE(.bam_warn_state[[id]])) {
    return(invisible(FALSE))
  }
  .bam_warn_state[[id]] <- TRUE
  warning(..., call. = FALSE)
  invisible(TRUE)
}

# Use matrix to check species availabilities per bcr
.filter_species_by_bcr  <- function(birdlist, spList, bcrNM) {
  valid_sp <- intersect(spList, names(birdlist))

  # subset birdlist for selected BCRs
  subset <- birdlist[birdlist$bcr %in% bcrNM, c("bcr", valid_sp), drop = FALSE]

  mat <- as.matrix(subset[valid_sp])
  keep <- colSums(mat) > 0
  valid_sp[keep]
}


# Assign bcr zone.
.bcr_regions <- list(
  Alaska = c("usa2", "usa4-0", "usa4-1", "usa4-2", "usa5", "Alaska"),
  Canada = c("can10", "can11", "can12", "can13", "can14", "can3", "can4-0",
             "can4-3", "can4-4", "can5", "can71", "can72", "can73", "can74",
             "can75", "can76", "can77-0", "can77-1", "can9", "Canada"),
  Lower48 = c("usa5", "usa9", "usa10", "usa11", "usa12", "usa13", "usa14",
              "usa23", "usa28", "usa30", "Lower48")
)

.get_region <- function(bcrNM) {
  bcrNM <- unique(bcrNM)

  matches <- lapply(
    bcrNM,
    function(x) names(.bcr_regions)[
      vapply(.bcr_regions, function(region_bcrs) x %in% region_bcrs, logical(1))
    ]
  )

  unknown <- bcrNM[lengths(matches) == 0L]
  if (length(unknown) > 0L) {
    stop("Unknown BCR(s): ", paste(unknown, collapse = ", "))
  }

  # A valid request must have at least one geographic region in common.
  valid_regions <- Reduce(intersect, matches)

  if (length(valid_regions) == 0L) {
    stop(
      "BCRs do not belong to a common geographic region: ",
      paste(bcrNM, collapse = ", ")
    )
  }

  if (length(valid_regions) > 1L) {
    stop(
      "Cannot infer one geographic region from: ",
      paste(bcrNM, collapse = ", "),
      ". Specify a region explicitly."
    )
  }

  valid_regions
}

# get file name and URL
.get_file_info <- function(url, version, species_code, bcrNM, year) {
  if (version == "v4") {
    file_name <- paste0("WeightedMosaic_", species_code, ".tiff")
    file_url <- file.path(url, file_name)
  } else if (version == "v5") {
    region <- if (length(bcrNM) == 1L) bcrNM else .get_region(bcrNM)
    file_name <- paste0(species_code, "_", region, "_", year, ".tif")
    file_url <- file.path(url, species_code, region, file_name)
  }
  list(name = file_name, url = file_url)
}

# download raster to a file
.download_raster <- function(file_url, destfile) {
  target_file <-  file.path(destfile, basename(file_url))
  writeBin(content(GET(file_url), "raw"), target_file)
  rast(target_file)
}

#function crop raster to extent
.crop_raster <- function(r, ext) {
  r_proj <- terra::project(ext, r)
  terra::crop(r, r_proj, snap = "near", mask = TRUE)
}

.batch_download <- function(species_code, version, year = NULL, crop_ext, bcrNM = "Canada", destfile) {
  message("Downloading data for ", species_code, " from version ", version)

  # Get file info
  url <- version.url$url[version.url$version == version]
  file_info <- .get_file_info(url, version, species_code, bcrNM, year)
  file_name <- file_info$name
  file_url  <- file_info$url

  # create output name
  out_name <- paste0(tools::file_path_sans_ext(file_name), ".tif")

  # Main raster loading
  if (!is.null(crop_ext)) {
    tiff_data <- .download_raster(file_url, destfile)

    tiff_data <- if (inherits(crop_ext, "SpatVector")) {
      .crop_raster(tiff_data, crop_ext)
    } else {
      .crop_raster(tiff_data, project(crop_ext, tiff_data, align_only = TRUE))
    }

    out_name <- sub("\\.tif?$", "_clip.tif", file_name)

  } else if (any(c("Canada", "Lower48", "Alaska") %in% bcrNM)) {
    tiff_data <- .download_raster(file_url, destfile)

  } else if(length(bcrNM)>1 || (length(bcrNM) == 1 && version == "v4")){
    tiff_mosaic <- .download_raster(file_url, destfile)

    extent <- system.file(
      "extdata",
      ifelse(version == "v4", "BAM_BCRNMv4_3978.shp", "BAM_BCRNMv5_3978.shp"),
      package = "BAMexploreR"
    ) |> vect()
    extent <- extent[extent$bcr %in% bcrNM, ]
    tiff_data <- .crop_raster(tiff_mosaic, extent)

    if (version == "v4"){
      out_name <- paste0(species_code, "-CAN-Mean_BCRclip.tif")
    }else{
      out_name <- paste0(species_code, "_BCRclip_", year, ".tif")
    }
  } else {
    tiff_data <- .download_raster(file_url, destfile)
  }

  if (!terra::same.crs(tiff_data, "EPSG:3978"))
    tiff_data <- terra::project(tiff_data, "EPSG:3978")

  if(isFALSE(sources(tiff_data) == file.path(destfile, out_name))){
    terra::writeRaster(tiff_data, file.path(destfile, out_name), overwrite = TRUE)
  }

  if (exists("tiff_mosaic")) {file.remove(sources(tiff_mosaic))}

  return(setNames(list(tiff_data), species_code))
}


#' Load and prepare a predictor-importance dataset
#'
#' Internal helper shared by \code{bam_predictor_importance()} and
#' \code{bam_predictor_barchart()}.
#'
#' Relative influence is normalised by \code{gbm::summary.gbm()} to sum to 100
#' within a single model, i.e. within a species x BCR x bootstrap. The v5 export
#' preserves this: every species x BCR sums to exactly 100. The v4 export was
#' truncated to predictors with \code{rel.inf >= 1}, so its species x BCR totals
#' range from ~1 to ~88 and are not comparable to one another. For v4 we rescale
#' each species x BCR back to 100 so that units are at least internally
#' consistent, and warn that composition remains distorted.
#'
#' @param version A \code{character}, either \code{"v4"} or \code{"v5"}.
#'
#' @return A \code{data.frame} of predictor importance whose \code{mean_rel_inf}
#'   sums to 100 within each species x BCR.
#'
#' @importFrom dplyr group_by mutate ungroup select
#' @noRd
.load_predictor_importance <- function(version) {

  if (version == "v5") {
    return(bam_predictor_importance_v5)
  }

  .warn_once(
    "v4_truncated",
    "Version 'v4' predictor importance retains only predictors with a relative ",
    "influence >= 1 (a median of 2 predictors per species x BCR). Each species x ",
    "BCR has been rescaled to sum to 100, but the truncation still biases ",
    "composition toward predictor classes made up of few, strongly influential ",
    "predictors. Treat cross-species and cross-BCR comparisons as indicative only; ",
    "use version = 'v5' where possible."
  )

  # rescale each species x BCR to sum to 100, carrying sd_rel_inf on the same scale
  bam_predictor_importance_v4 |>
    dplyr::group_by(spp, bcr) |>
    dplyr::mutate(
      .scale       = 100 / sum(mean_rel_inf, na.rm = TRUE),
      mean_rel_inf = mean_rel_inf * .scale,
      sd_rel_inf   = sd_rel_inf * .scale
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.scale)
}


#' Bootstrap-level predictor-class shares, derived on demand
#'
#' Correct uncertainty for a predictor class requires summing relative influence
#' within that class \emph{inside each bootstrap} before taking any variance,
#' because relative influence is compositional: predictors within one model are
#' negatively correlated by construction. The species x BCR summary datasets have
#' already averaged over bootstraps and cannot support this.
#'
#' \code{bam_predictor_boot_v5} is the single source of truth: it holds
#' \code{rel.inf} for every species x BCR x bootstrap x predictor, and
#' \code{bam_predictor_importance_v5} is derived from it. This helper performs the
#' other derivation, rolling predictors up to their class within each bootstrap.
#' The result is memoised, since collapsing ~1.7 million rows is wasted work on
#' the second and subsequent call in a session.
#'
#' @param version A \code{character}, either \code{"v4"} or \code{"v5"}.
#'
#' @return A \code{data.frame} with columns \code{spp}, \code{bcr}, \code{boot},
#'   \code{predictor_class} and \code{share}, or \code{NULL} when no
#'   bootstrap-level dataset is shipped for this version (v4 has none).
#'
#' @importFrom dplyr filter group_by summarise arrange
#' @noRd
.predictor_class_boot <- function(version) {

  key <- paste0("class_boot_", version)
  if (!is.null(.bam_cache[[key]])) return(.bam_cache[[key]])

  raw <- .get_dataset(paste0("bam_predictor_boot_", version))
  if (is.null(raw)) return(NULL)

  # `rel.inf` sums to 100 within a species x BCR x bootstrap, so dividing the
  # class total by 100 gives that class's share of the model in that bootstrap.
  # Predictors with no class would silently deflate the shares, so drop them
  # first; the shipped v5 data has none.
  out <-
    raw |>
    dplyr::filter(!is.na(predictor_class)) |>
    dplyr::group_by(spp, bcr, boot, predictor_class) |>
    dplyr::summarise(share = sum(rel.inf) / 100, .groups = "drop") |>
    dplyr::arrange(spp, bcr, boot, predictor_class) |>
    as.data.frame()

  .bam_cache[[key]] <- out
  out
}
