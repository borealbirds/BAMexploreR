##################################################################################
#' Retrieve list of species
#'
#' The function produce a character vector of species available to download. To
#' retrieve available species, the function derive the list using the version, and type of output
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param type character; type of output provided in the list, either "species_code", "common_name", "family_name" or "scientific_name".
#' @param guild character; Specifies the guild to filter the species list, based on the classification used in The State of Canada’s Birds Report (Birds Canada, 2024).
#'              By providing a guild (e.g., "Forest Birds"), the function will return only the species available within that specific category.
#'              Accepted guild names include "Forest_Birds", "Grassland_Birds", "Waterfowl", and others as defined in the report.
#'
#' @return Vector of species name
#'
#' @importFrom httr GET content
#' @importFrom dplyr pull mutate filter
#' @importFrom stringr str_sub
#' @docType methods
#' @author Melina Houle
#' @rdname sppList
#' @export
#' @examples

#' speciesList <- sppList("v4", "mean", "species_code", guild)
sppList <- function(version, type, guild = NULL) {
  load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  spdt <- spp.List

  if(is.null(guild)){
    spcode <- spdt %>% dplyr::pull("code")
  }else if(any(!(guild %in% guild_opt))){
    print("Guild is invalid")
  }else{
    spcode <- spdt %>%
      dplyr::filter(if_any(all_of(guild), ~ . == 1)) %>%  # Use if_any to check across multiple columns
      dplyr::pull("code")  # Extract species code
  }

  url <- version.url$url[version.url$version == version]
  response <- httr::GET(url)
  content_text <- httr::content(response, "text")
  if (status_code(response) == 200) {
    if(version == "v4" || version == "v4_demo"){
      # Use regular expressions to parse
      tiff_files <- regmatches(content_text, gregexpr('href="([^"]+\\.tif)"', content_text))
      tiff_files <- unlist(tiff_files)
      tiff_files <- gsub('href="|/"', '', tiff_files)
      spList <- tiff_files %>%
        stringr::str_sub(start = 6, end = 9) %>%
        .[.%in%spcode]
      return(spList)
    } else if(version == "v5" || version == "v5_demo"){
      # Use regular expressions to parse
      subdirs <- regmatches(content_text, gregexpr('href="([^"]+/)"', content_text))
      subdirs <- unlist(subdirs)
      spList <- gsub('href="|/"', '', subdirs) %>%
        .[!(. %in% "/data")]
      return(spList)
    } else {
      print("You must specified either v4 or v5")
    }
  } else {
      # Return an error message if the request failed
      return(paste("Error:", status_code(response)))
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
