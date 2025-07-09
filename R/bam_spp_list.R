##################################################################################
#' Retrieve list of species
#'
#' The function produce a character vector of species available to download. To
#' retrieve available species, the function derive the list using the version, and type of output
#'
#' @param version character; Indicate the version of the National Model requested. Each version of the
#'        National Model has its url access provided within the package.
#' @param type character; type of output provided in the list, either \code{"speciesCode"}, \code{"commonName"} or \code{"scientificName"}.Default is \code{"speciesCode"}.
#' @param guild character; Specifies the guild to filter the species list, based on the classification used in The State of Canada’s Birds Report (Birds Canada, 2024).
#'              By providing a guild (e.g., "Forest Birds"), the function will return only the species available within that specific category.
#'              Accepted guild names include \code{"Forest_Birds"}, \code{"Grassland_Birds"}, \code{"Waterfowl"}, and others as defined in the report.
#'
#' @return Vector of species name
#'
#' @importFrom httr GET content
#' @importFrom dplyr pull mutate filter
#' @importFrom stringr str_sub
#' @importFrom tidyselect all_of
#'
#' @docType methods
#' @author Melina Houle
#' @rdname bam_spp_list
#' @export
#' @examples

#' speciesList <- bam_spp_list("v4", "speciesCode")
bam_spp_list <- function(version, type = "speciesCode", guild = NULL) {
  spdt <- spp_tbl

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }

  if (!type %in% c("speciesCode", "commonName", "scientificName")) {
    stop("Invalid type argument. Must be one of 'speciesCode', 'commonName' or 'scientificName'.")
  }

  if(is.null(guild)){
    spcode <- spdt %>% dplyr::pull("speciesCode")
  }else if(any(!(guild %in% guild_opt))){
    print("Guild is invalid")
  }else{
    spcode <- spdt %>%
      dplyr::filter(dplyr::if_any(tidyselect::all_of(guild), ~ . == 1)) %>%  # Use if_any to check across multiple columns
      dplyr::pull("speciesCode")  # Extract species code
  }

  url <- version.url$url[version.url$version == version]
  response <- httr::GET(url)
  content_text <- httr::content(response, "text")
  if (httr::status_code(response) == 200) {
    if(version == "v4"){
      # Use regular expressions to parse
      tiff_files <- regmatches(content_text, gregexpr('href="([^"]+\\.tif)"', content_text))
      tiff_files <- unlist(tiff_files)
      tiff_files <- gsub('href="|/"', '', tiff_files)
      spList <- tiff_files %>%
        stringr::str_sub(start = 6, end = 9) %>%
        .[.%in%spcode]
    } else if(version == "v5"){
      # Use regular expressions to parse
      subdirs <- regmatches(content_text, gregexpr('href="([^"]+/)"', content_text))
      subdirs <- unlist(subdirs)
      spList <- gsub('href="|/"', '', subdirs) %>%
        .[!(. %in% "/data")]
    } else {
      print("You must specified either v4 or v5")
    }
  } else {
      # Return an error message if the request failed
      return(paste("Error:", httr::status_code(response)))
  }

  # Extract species list
  if(type=="speciesCode"){
    sp <-spList
  }else if(type=="commonName") {
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList) %>%
      dplyr::pull(commonName)
  }else if(type=="scientificName") {
    sp <- spdt %>%
      dplyr::filter(speciesCode %in% spList) %>%
      dplyr::pull(scientificName)
  }
 sp <- unique(sp)
 return(sp)
}
