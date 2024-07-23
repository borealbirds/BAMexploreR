##################################################################################
#' Retrieve list of species
#'
#' The function produce a character vector of species available to download. To
#' retrieve available species, the function derive the list using the version, and type of output
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#'
#' @param layer character; Name of the output layers of interest, either "mean", "sd", "overextrapolated", "ptdensity" or "distnear".
#'
#' @param type character; type of output provided in the list, either "species_code", "common_name", "family_name" or "scientific_name".
#'
#' @return Vector of available url to download
#'
#' @importFrom googledrive drive_ls
#' @import dplyr
#' @importFrom stringr str_sub
#' @docType methods
#' @author Melina Houle
#' @rdname sppList
#' @export
#' @examples

#' library(data.table)
#' dt<- data.table(dataset = c("NFDB"),
#'                url = c("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/"),
#'                password= c(NA))
#' listWebData(dt, datasetName = "NFDB")
sppList <- function(version, layer, out_format) {
  load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  #spdt <- get(data(spp.List))
  spdt <- spp.List
  #pid <- get(data(version.url))
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
  if(out_format=="species_code"){
    sp <-spList
  }else if(out_format=="commonName") {
    sp <- spdt %>%
      filter(code %in% spList) %>%
      pull(commonName)
  }else if(out_format=="order") {
    sp <- spdt %>%
      filter(code %in% spList) %>%
      pull(order)  %>%
      unique()
  }else if(out_format=="scientificName") {
    sp <- spdt %>%
      filter(code %in% spList) %>%
      pull(scientificName)
  }
 sp <- unique(sp)
 return(sp)
}
