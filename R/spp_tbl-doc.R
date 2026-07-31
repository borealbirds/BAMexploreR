#' Table of BAM species
#'
#' This dataset lists the union of species available for the current and archived
#' BAM Landbird Density & Habitat models. Current-model names and taxonomy come
#' from the Version 5 results workbook. Species-group categories are retained from
#' the State of Canada's Birds classifications used by the previous table.
#' Classification fields are \code{NA} for current-model species that were not
#' present in the previous table.
#' The same species can be included in more than one group, but only species that
#' are truly representative of a given group are included in each
#' (Birds Canada and Environment and Climate Change Canada. 2024. The State of Canada’s Birds Report. Accessed from NatureCounts. DOI: 10.71842/8bab-ks08)
#'
#' @format A data frame with 151 rows and 20 columns:
#' \describe{
#'   \item{speciesCode}{AOU code used by WildTrax.}
#'   \item{commonName}{Common name of the bird.}
#'   \item{frenchName}{French common name of the bird, when available.}
#'   \item{scientificName}{Scientific name of the species.}
#'   \item{family}{Taxonomic family from the Version 5 results workbook, when available.}
#'   \item{order}{Taxonomic order of the species.}
#'   \item{v4}{Binary value indicating whether the species is included in the archived models.}
#'   \item{v5}{Binary value indicating whether the species is included in the current models.}
#'   \item{COSEWIC}{Binary value (0 or 1) indicating whether the species is listed under COSEWIC (1 = listed, 0 = not listed).}
#'   \item{Cavity_Birds}{Binary value (0 or 1) indicating whether the species is classified as a cavity-nesting bird (1 = yes, 0 = no).}
#'   \item{Waterfowl}{Binary value (0 or 1) indicating whether the species is classified as waterfowl (1 = yes, 0 = no).}
#'   \item{Marine_Birds}{Binary value (0 or 1) indicating whether the species is classified as a marine bird (1 = yes, 0 = no).}
#'   \item{Shorebirds}{Binary value (0 or 1) indicating whether the species is classified as a shorebird (1 = yes, 0 = no).}
#'   \item{Wetland_Birds}{Binary value (0 or 1) indicating whether the species is classified as a wetland bird (1 = yes, 0 = no).}
#'   \item{Birds_of_Prey}{Binary value (0 or 1) indicating whether the species is classified as a bird of prey (1 = yes, 0 = no).}
#'   \item{Forest_Birds}{Binary value (0 or 1) indicating whether the species is classified as a forest bird (1 = yes, 0 = no).}
#'   \item{Grassland_Birds}{Binary value (0 or 1) indicating whether the species is classified as a grassland bird (1 = yes, 0 = no).}
#'   \item{Aerial_Insectivores}{Binary value (0 or 1) indicating whether the species is classified as an aerial insectivore (1 = yes, 0 = no).}
#'   \item{Arctic_Birds}{Binary value (0 or 1) indicating whether the species is classified as an Arctic bird (1 = yes, 0 = no).}
#'   \item{Long_Distance_Migrants}{Binary value (0 or 1) indicating whether the species is classified as a long-distance migrant (1 = yes, 0 = no).}
#' }
#' @keywords internal
#'
#' @docType data
"spp_tbl"
