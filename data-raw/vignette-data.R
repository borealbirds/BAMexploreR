library(dplyr)

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
usethis::use_data(version.url, spp.List, internal = TRUE, overwrite = TRUE)
