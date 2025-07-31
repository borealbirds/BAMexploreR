#' Standardize user's species inputs to 4-letter bird codes
#' 
#' Internal helper function. Converts species names (common, scientific, or FLBCs)
#' to FLBCs using spp_tbl.
#' 
#' @param species_input A character vector of species names or codes.
#' @param spp_tbl A lookup table containing speciesCode, commonName, and scientificName.
#'
#' @return A character vector of species codes (same length as input).
#' @noRd

standardize_species_names <- function(species_input, spp_tbl) {
  
  # convert to lowercase for case-insensitive matching
  species_input_lower <- tolower(species_input)
  
  # also make lookup columns lowercase
  spp_tbl <- 
    spp_tbl |> 
    mutate(
      speciesCode_lower = tolower(speciesCode),
      commonName_lower = tolower(commonName),
      scientificName_lower = tolower(scientificName)
    )
  
  # convert users' species to FLBCs
  matched_codes <- 
    purrr::map_chr(species_input_lower, function(sp) {
      
    if (sp %in% spp_tbl$speciesCode_lower) {
      
      spp_tbl$speciesCode[match(sp, spp_tbl$speciesCode_lower)]
      
    } else if (sp %in% spp_tbl$commonName_lower) {
      
      spp_tbl$speciesCode[match(sp, spp_tbl$commonName_lower)]
      
    } else if (sp %in% spp_tbl$scientificName_lower) {
      
      spp_tbl$speciesCode[match(sp, spp_tbl$scientificName_lower)]
      
    } else {
      warning(paste0(sp, "not found in spp_tbl. Returning NA."))
      NA_character_
    }
  }) # close map_chr()

  return(matched_codes)
  
} # close function
