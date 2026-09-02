if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".env","predictor_class", "mean_rel_inf", "sd_rel_inf", "species", "spp", "sum_inf", "sum_all_groups",
                           "pooled_sd", "percent_inf", "sym", "sd_percent_inf", "guild_opt", "speciesCode", "commonName",
                           "scientificName", "sum_influence", "sum_group1", "prop", "density", "spp_tbl", "bam_predictor_response_v5"))
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
