library(tibble)
library(readxl)
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
### create 2nd internal data: most updated species list
###############################################################
# species list URL data file
spp_tbl <- read.csv("./data-raw/sppList.csv", header= TRUE)
use_data(spp_tbl, internal = FALSE, overwrite = TRUE)


###############################################################
### create 3rd internal data: version 4 model covariate importance
###############################################################
load("./data/bam_predictor_importance_v4.rda")
load("./data/bam_predictor_importance_v5.rda")

###############################################################
### create 4th internal data: birdlist matrix to indicatespecies avaiable per BCR
###############################################################
birdlist <- readRDS("./data-raw/birdlist.rds")

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
