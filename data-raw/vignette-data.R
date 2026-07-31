library(tibble)
library(readxl)
library(dplyr)
library(usethis)
###############################################################
### create 1st internal data: URL for version 4/5
###############################################################
# version URL data file
version.url <- tibble(
  version = c("v4",
              "v5"),
  url = c("http://206.12.92.143/data/NationalModelv4",
          "http://206.12.92.143/data/NationalModelv5")
)

###############################################################
### create 2nd internal data: species metadata for available model releases
###############################################################
# Preserve Version 4 metadata and classifications from the existing table.
spp_v4 <- read.csv("./data-raw/sppList.csv", header = TRUE)
names(spp_v4)[names(spp_v4) == "Cavity"] <- "Cavity_Birds"
spp_v4 <- spp_v4 |>
  mutate(v4 = 1L)

# Version 5 species metadata extracted from the `species` sheet of
# G:/Shared drives/BAM_NationalModels5/distribution/BAMV5-results.xlsx.
spp_v5 <- read.csv("./data-raw/sppList_v5.csv", header = TRUE) |>
  transmute(
    speciesCode = id,
    scientificName = scientific,
    commonName = english,
    frenchName = french,
    family,
    v5 = 1L
  )

# Use Version 5 names and taxonomy where available, while retaining Version
# 4-only species and the existing State of Canada's Birds classifications.
spp_tbl <- full_join(spp_v4, spp_v5, by = "speciesCode", suffix = c("_v4", "_v5")) |>
  transmute(
    speciesCode,
    commonName = coalesce(commonName_v5, commonName_v4),
    frenchName,
    scientificName = coalesce(scientificName_v5, scientificName_v4),
    family,
    order,
    v4 = coalesce(v4, 0L),
    v5 = coalesce(v5, 0L),
    COSEWIC,
    Cavity_Birds,
    Waterfowl,
    Marine_Birds,
    Shorebirds,
    Wetland_Birds,
    Birds_of_Prey,
    Forest_Birds,
    Grassland_Birds,
    Aerial_Insectivores,
    Arctic_Birds,
    Long_Distance_Migrants
  ) |>
  arrange(speciesCode)

use_data(spp_tbl, internal = FALSE, overwrite = TRUE)


###############################################################
### create 3rd internal data: version 4 model covariate importance
###############################################################
load("./data/bam_predictor_importance_v4.rda")
load("./data/bam_predictor_importance_v5.rda")
load("./data/bam_predictor_response_v5.rda")

###############################################################
### create 4th internal data: birdlist matrix indicating species available per BCR
###############################################################
if (file.exists("./data-raw/birdlist.rds")) {
  birdlist <- readRDS("./data-raw/birdlist.rds")
} else {
  existing_internal_data <- new.env(parent = emptyenv())
  load("./R/sysdata.rda", envir = existing_internal_data)
  birdlist <- existing_internal_data$birdlist
}

###############################################################
### Generate internal data
###############################################################
use_data(version.url, spp_tbl, bam_predictor_importance_v4, bam_predictor_importance_v5, birdlist, internal = TRUE, overwrite = TRUE)

###############################################################
### Generate external data
###############################################################
# guild list URL data file
guild_opt <- c("COSEWIC",
               "Cavity_Birds",
               "Waterfowl",
               "Marine_Birds",
               "Shorebirds",
               "Wetland_Birds",
               "Birds_of_Prey",
               "Forest_Birds",
               "Grassland_Birds",
               "Aerial_Insectivores",
               "Arctic_Birds",
               "Long_Distance_Migrants")

use_data(guild_opt, internal = FALSE, overwrite = TRUE)
