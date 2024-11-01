##################################################################################
#' Retrieve list of species
#'
#' The function produce a character vector of species available to download. To
#' retrieve available species, the function derive the list using the version, and type of output
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#'
#' @param layer character; Name of the output layers of interest, either "mean", "sd" or "overextrapolated".
#'
#' @param type character; type of output provided in the list, either "species_code", "common_name", "family_name" or "scientific_name".
#'
#' @return Vector of available url to download
#'
#' @importFrom googledrive drive_ls drive_auth drive_has_token
#' @importFrom dplyr pull mutate filter
#' @importFrom stringr str_sub
#' @docType methods
#' @author Melina Houle
#' @rdname sppList
#' @export
#' @examples

#' speciesList <- sppList("v4", "mean", "species_code")
sppList <- function(version, layer, type) {
  #browser()
  #googledrive::drive_auth()
  if (!googledrive::drive_has_token()) {
    googledrive::drive_auth()
  }
  load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  spdt <- spp.List
  pid <- version.url
  gd.list <- googledrive::drive_ls(pid$url[pid$version == version])
  spcode <- spdt %>% dplyr::pull("code")
  if(version == "v4"){
    spList <- gd.list %>%
      dplyr::mutate(codesp = stringr::str_sub(name, start = 6, end = 9)) %>%
      dplyr::filter(codesp %in% spcode) %>%  # Filter for desired substrings
      dplyr::pull(codesp)
  } else if(version == "v5"){
    spList <- gd.list %>%
      dplyr::mutate(codesp = stringr::str_sub(name, start = 1, end = 4)) %>%
      dplyr::filter(codesp %in% spcode) %>%  # Filter for desired substrings
      dplyr::pull(codesp)
  } else {
    print("You must specified either v4 or v5")
  }
  # Extract species list
  if(type=="species_code"){
    sp <-spList
  }else if(type=="commonName") {
    sp <- spdt %>%
      dplyr::filter(code %in% spList) %>%
      dplyr::pull(commonName)
  }else if(type=="order") {
    sp <- spdt %>%
      dplyr::filter(code %in% spList) %>%
      dplyr::pull(order)  %>%
      unique()
  }else if(type=="scientificName") {
    sp <- spdt %>%
      dplyr::filter(code %in% spList) %>%
      dplyr::pull(scientificName)
  }
 sp <- unique(sp)
 return(sp)
}
