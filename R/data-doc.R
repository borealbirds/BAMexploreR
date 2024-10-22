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
