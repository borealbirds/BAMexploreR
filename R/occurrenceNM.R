##################################################################################
#' Estimate a presence/absence threshold using a Lorenz curve
#'
#' This function fits a Lorenz curve to a density raster. The x-axis represents the cumulative
#' proportion of area (sorted by bird density, \code{p}) and the y-axis represents the cumulative
#' proportion of the raster's total population (\code{L}). If bird density were uniform across space, the
#' plot would follow a line with a slope of 1. In reality birds are not distributed evenly
#' on the landscape. This function uses \code{opticut::lorenz()} to estimate a
#' pixel density threshold separating "presence" from "absence". The threshold is defined as
#' the point on the Lorenz curve where the tangent has a slope of 1 and \code{p - L}
#' is maximized. See \code{?opticut::lorenz} for details.
#'
#'
#'
#'@param raster A raster of the "Area of Interest". Defined and created via Melina's function(s).
#'
#'@param quantile Default is \code{NULL}, and the  optimum threshold is estimated via
#'\code{opticut::lorenz()} as the pixel density when the slope of the tangent of the Lorenz function is 1.
#'If a custom threshold is preferred, set this argument using a \code{numeric} between 0 and 1.
#'Indicates a cumulative proportion of pixels, above which all raster pixel values are assigned as "presence".
#'Raster pixel values below the quantile are assigned "absent". E.g. By setting \code{quantile=0.8},
#'the threshold density for separating presence versus absence is whatever pixel value accumulates 80% of the total
#'pixels from the raster.
#'
#'@param plot Default is \code{TRUE} for visualizing occurrence patterns.
#'
#' @return A list with:
#' \describe{
#'   \item{raster}{A \code{SpatRaster} with binary presence (1) / absence (0) values.}
#'   \item{threshold}{A \code{numeric} value: the estimated density threshold.}
#'   \item{population_summary}{A \code{data.frame} with estimated total population size,
#'   mean, and standard deviation, before and after thresholding.
#'   See: \code{BAMexploreR::pop_sizeNM()}}
#' }
#'
#'@importFrom opticut lorenz quantile.lorenz
#'@importFrom terra rast values
#'
#'@export
#'@examples
#' # find Tennessee Warbler core habitat
#' tewa <- getlayerNM("TEWA", "v4", destfile = tempdir())
#' occurrenceNM(tewa$TEWA)

occurrenceNM <- function(raster, quantile=NULL, plot=TRUE){


  # retrieve density per pixel, retain this vector with NAs to preserve pixel positions
  dpp <- terra::values(raster, na.rm=FALSE)

  # remove NAs for `lorenz()`
  dpp_no_nas <- dpp[!is.na(dpp)]

  # find optimal presence/absence threshold

  # estimate threshold from a Lorenz curve
  lorenz_fit <- opticut::lorenz(dpp_no_nas)

    if (is.null(quantile)){

      # locate the pixel value where the tangent approaches 1:1
      t_pixel <- summary(lorenz_fit)["t"]

      # "x" is the bird density when "t" approaches 1:1
      # ("p" is the proportion of pixels, "L" is the proportion of birds)
      optimum_threshold <- lorenz_fit[t_pixel, "x"]
      names(optimum_threshold) <- "optimum threshold"

    } else {

      # `L` for ordered cumulative abundance quantiles (versus non-cumulative)
      # `threshold` partitions "1-threshold" proportion of values as presence (1) and the rest ("threshold") as absence (0)
      # e.g. for `threshold=0.8` the densest 20% of values are assigned as presence (1) and the rest as absence (0)
      optimum_threshold <- opticut::quantile.lorenz(lorenz_fit, probs = quantile, type = "L")
      names(optimum_threshold) <- "optimum threshold"

    } # finish finding optimum threshold

  # create binarized density raster, i.e.
  # assign pixels 1 or 0 based on the current threshold
  # note: need to preserve NA positions from the original raster to
  # maintain raster range between input and output
  classify_pixels <- function(dpp, threshold) {
    ifelse(dpp >= threshold, 1, ifelse(!is.na(dpp), 0, NA))
  }

  pixels_binary <- classify_pixels(dpp, optimum_threshold)

  # write the binary values into a raster object
  binary_raster <- terra::setValues(raster, pixels_binary)

  # mask the original raster with above threshold values
  threshold_raster <- terra::setValues(raster, ifelse(dpp >= optimum_threshold, dpp, 0))

  # plot occurrence map
  if (plot==TRUE){
    terra::plot(binary_raster, main=paste("density threshold =", round(optimum_threshold, digits = 5)))
  }

  # estimate impact of thresholding on population estimate
  og_pop_size <- BAMexploreR::pop_sizeNM(raster) |> mutate(type = "no_threshold")
  threshold_pop_size <- BAMexploreR::pop_sizeNM(threshold_raster) |> mutate(type = "with_threshold")

  pop_summary <- bind_rows(og_pop_size, threshold_pop_size)

  return(list(raster = binary_raster, threshold = optimum_threshold, population_summary = pop_summary))

}

