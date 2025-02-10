#' Species Group Data
#'
#' This dataset contains a vector of species groups used in analysis.
#' The groups include categories such as "Waterfowl", "Birds of Prey", and "Shorebirds".
#'
#' @format A character vector with 12 elements:
#' \describe{
#'   \item{COSEWIC_Status}{The conservation status of species.}
#'   \item{Cavity_Birds}{Species that nest in cavities.}
#'   \item{Waterfowl}{Bird species that primarily live on or near water.}
#'   \item{Marine_Birds}{Bird species that live in marine environments.}
#'   \item{Shorebirds}{Bird species typically found along shorelines.}
#'   \item{Wetland_Birds}{Bird species that inhabit wetlands.}
#'   \item{Birds_of_Prey}{Raptors or predatory birds.}
#'   \item{Forest_Birds}{Bird species that live in forested areas.}
#'   \item{Grassland_Birds}{Bird species that inhabit grasslands.}
#'   \item{Aerial_Insectivores}{Birds that feed on insects while flying.}
#'   \item{Arctic_Birds}{Bird species found in Arctic regions.}
#'   \item{Long_Distance_Migrants}{Birds that migrate long distances between breeding and wintering grounds.}
#' }
#'
#' @source The data was derived from internal project datasets and species grouping systems.
#' @keywords datasets
#' @examples
#' data(guild_opt)
#' head(guild_opt)
"guild_opt"

#' BAM Covariate Importance Data (v4)
#'
#' `bam_covariate_importance_v4` is a `data.frame` where each row represents the mean relative influence (calculated from 32 bootstraps) of a model covariate for a given species × Bird Conservation Region (BCR).
#'
#' @format A `data.frame` with 7 columns:
#' \describe{
#'   \item{spp}{A four-letter bird code indicating the species.}
#'   \item{bcr}{The Bird Conservation Region.}
#'   \item{var}{The name of the covariate.}
#'   \item{mean_rel_inf}{The mean relative influence of the covariate across 32 bootstraps.}
#'   \item{sd_rel_inf}{The standard deviation of the covariate's relative influence across 32 bootstraps.}
#'   \item{n_boots}{The number of bootstraps in which the given covariate appeared.}
#'   \item{var_class}{A broad variable class to which the covariate belongs.}
#' }
#'
#' @source The data was derived from internal project datasets.
#' @keywords datasets
#' @examples
#' data(bam_covariate_importance_v4)
#' head(bam_covariate_importance_v4)
"bam_covariate_importance_v4"

#' spp_List
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
"spp_List"

