#' Table of BAM species
#'
#' This dataset lists all species from which BAM National density model were generated. We applied species groups categories used by
#' The State of the Canada's birds who classify species according to broad biomes and groups of species that are known to have distinct and noteworthy trends.
#' The same species can be included in more than one group, but only species that are truly representative of a given group are included in each
#' (Birds Canada and Environment and Climate Change Canada. 2024. The State of Canada’s Birds Report. Accessed from NatureCounts. DOI: 10.71842/8bab-ks08)
#'
#' @format A data frame with 143 rows and 16 columns:
#' \describe{
#'   \item{speciesCode}{AOU code used by WildTrax.}
#'   \item{commonName}{Common name of the bird.}
#'   \item{order}{Taxonomic order of the species.}
#'   \item{scientificName}{Scientific name of the species.}
#'   \item{COSEWIC}{Binary value (0 or 1) indicating whether the species is listed under COSEWIC (1 = listed, 0 = not listed).}
#'   \item{Cavity}{Binary value (0 or 1) indicating whether the species is classified as a cavity-nesting bird (1 = yes, 0 = no).}
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
