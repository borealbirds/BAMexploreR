library(dplyr)
library(readxl)
library(tidyr)

version.url <- tibble(
  version = c("v4_complete",
              "v5_complete",
              "v4",
              "v5"),
  url = c("https://drive.google.com/drive/u/0/folders/1aJUZr4fACdD02H8AYejR2XG6zuA6E492",
          "https://drive.google.com/drive/u/0/folders/1hYkgS0mTIqv7ZWE7t_3TJxsGosQY9eFx",
          "https://drive.google.com/drive/folders/1bVBEABbrcfgHidmo_PLue916Knjmayre",
          "https://drive.google.com/drive/folders/1J-V3cdkFYlLolZ53Sy_8tgTDrXLb9xux")
)

spp.List <- read.csv("./data-raw/sppList.csv", header= TRUE)



file_path <- "./data-raw/NationalModels_V5_VariableList.xlsx"
v1 <- read_excel(file_path, sheet = 1)
v3 <- read_excel(file_path, sheet = 3)

covariates_label <- v3 %>%
  select(Label, Category, Extent, Source, GapCategory) %>%
  separate(Label, into = c("Label1", "res"), sep = "_", remove = FALSE) %>%
  inner_join(v1, by = c("Label1" = "Abbreviation")) %>%
  select(Label, Variable, Category, Extent, Source, GapCategory)


usethis::use_data(version.url, spp.List, covariates_label, internal = TRUE, overwrite = TRUE)


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

usethis::use_data(guild_opt, internal = FALSE, overwrite = TRUE)
