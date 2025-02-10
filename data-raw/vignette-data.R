library(tibble)
library(readxl)
library(usethis)

###############################################################
### create 1st internal data: URL for version 4/5
###############################################################
# version URL data file
version.url <- tibble(
  version = c("v4_demo",
              "v5_demo",
              "v4",
              "v5",
              "v4_GD",
              "v5_GD"),
  url = c("http://206.12.92.143/data/NationalModelv4_sample",
          "http://206.12.92.143/data/NationalModelv5",
          "http://206.12.92.143/data/NationalModelv4",
          "",
          "https://drive.google.com/drive/u/1/folders/1aJUZr4fACdD02H8AYejR2XG6zuA6E492",
          "https://drive.google.com/drive/u/1/folders/1snHdBwcVyUYCbwYJSCWm5506fiBEYal-")
)

###############################################################
### create 2nd internal data: most updated species list
###############################################################
# species list URL data file
spp_List <- read.csv("./data-raw/spp_List.csv", header= TRUE)
use_data(spp_List, internal = FALSE, overwrite = TRUE)
###############################################################
### create 3st internal data: version 4 model covariate importance
###############################################################
load("./data/bam_covariate_importance_v4.rda")

###############################################################
### create 4th internal data: Covariate LookUp table
###############################################################
#

###############################################################
### Generate internal data
###############################################################
usethis::use_data(version.url, bam_covariate_importance_v4, bam_covariate_importance_list_v4, internal = TRUE, overwrite = TRUE)


###############################################################
### Generate external data
###############################################################
# guild list URL data file
guild_opt <- c("COSEWIC_Status",
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
