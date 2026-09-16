#' Predictor metadata for BAM Landbird Density & Habitat models
#'
#' This dataset lists the variables used in the archived (Version 4) and current
#' (Version 5) BAM Landbird Density & Habitat models.
#' The table contains information on the definitions of each variable and the source
#' For the current models, the table also contains further details on the source,
#' as well as some of the methods used to extract each variable and build the raster
#' layers for model prediction.
#'
#' @format A data frame with 326 rows and 12 columns:
#' \describe{
#'   \item{version}{BAM Landbird Density & Habitat model release}
#'   \item{predictor}{Predictor name used in the model objects and output}
#'   \item{definition}{A description of the predictor}
#'   \item{predictor_class}{Broad class the predictor belongs to; used as a grouping variable in some of the package functions}
#'   \item{resolution}{The native spatial resolution of the data source}
#'   \item{source}{The original data source for the predictor}
#'   \item{provider}{The producer of the predictor}
#'   \item{citation}{Citation for the data source}
#'   \item{covariate_extraction}{The scale that the covariate was extracted from the native resolution of the datasource; numerical values indicate the buffer radius used for extraction}
#'   \item{prediction_resolution}{The resolution that the predictor was calculated at for model prediction; "1 kilometre" indicates the mean or mode within 1 km, "5x5 convolution kernel" indicates a moving focal window over the 1 km layer}
#'   \item{years}{The years of data available for the source predictor}
#'   \item{temporal_matching}{How the years of data were temporally matched to the bird survey data}
#' }
#' @keywords datasets
#'
#' @docType data
"predictor_metadata"
