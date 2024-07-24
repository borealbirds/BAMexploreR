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
#' @importFrom googledrive drive_ls drive_auth
#' @import dplyr
#' @importFrom stringr str_sub
#' @docType methods
#' @author Melina Houle
#' @rdname sppList
#' @export
#' @examples

#' speciesList <- sppList("v4", "mean", "species_code")
sppList <- function(version, layer, type) {
  load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  spdt <- spp.List
  pid <- version.url
  gd.list <- googledrive::drive_ls(pid$url[pid$version == version])
  spcode <- spdt %>% pull("code")
  if(version == "v4"){
    spList <- gd.list %>%
      mutate(codesp = stringr::str_sub(name, start = 6, end = 9)) %>%
      filter(codesp %in% spcode) %>%  # Filter for desired substrings
      pull(codesp)
  } else if(version == "v5"){
    spList <- gd.list %>%
      mutate(codesp = stringr::str_sub(name, start = 1, end = 4)) %>%
      filter(codesp %in% spcode) %>%  # Filter for desired substrings
      pull(codesp)
  } else {
    print("You must specified either v4 or v5")
  }
  # Extract species list
  if(type=="species_code"){
    sp <-spList
  }else if(type=="commonName") {
    sp <- spdt %>%
      filter(code %in% spList) %>%
      pull(commonName)
  }else if(type=="order") {
    sp <- spdt %>%
      filter(code %in% spList) %>%
      pull(order)  %>%
      unique()
  }else if(type=="scientificName") {
    sp <- spdt %>%
      filter(code %in% spList) %>%
      pull(scientificName)
  }
 sp <- unique(sp)
 return(sp)
}
