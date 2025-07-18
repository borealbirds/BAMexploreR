# BAMexploreR <img src="man/figures/logo.png" width="50%" align="right"/>

## Overview

`BAMexploreR` is an R package is a package for downloading and analyzing landbird density models produced by the Boreal Avian Modelling Centre (BAM).

The BAM landbird density models are species-specific predictions of the density of breeding male birds per hectare at a 1km resolution across the boreal forest. They are produced with a generalized analytical approach to model landbird species density in relation to environmental predictors, using in-person or ARU point-count surveys and widely available spatial predictors. We developed separate models for each geographic region (bird conservation regions) based on predictors such as tree species biomass (local and landscape scale), forest age, topography, land use, and climate. We used machine learning to allow for predictor interactions and non-linear responses while avoiding time-consuming species-by-species parameterization. We applied cross-validation to avoid overfitting and bootstrap resampling to estimate uncertainty associated with our density estimates.

``` r

# Construct direct download link from Google Drive file ID
file_id <- "1t9nNCmLoJV4CQoSR3Q_DCZ_CFrb-m3Pr"
img_file <- tempfile(fileext = ".png")  # Create a temporary file with .png extension

# Use the Google Drive direct download URL
download.file(
  url = paste0("https://drive.google.com/uc?export=download&id=", file_id),
  destfile = img_file,
  mode = "wb"
)

# Display the image
knitr::include_graphics(img_file)
```

Two versions of the BAM landbird density models are available in `BAMExploreR`. 

| Feature                                                             | BAM V4                                                | BAM V5                                                                 |
|---------------------------------------------------------------------|--------------------------------------------------------|------------------------------------------------------------------------|
| **Release year**                                                   | 2020                                                 | 2025 |
| **Species included**                                               | 143                                                  | 70 priority species (remaining 73 in progress) |
| **Dataset size**                                                   | 0.3 million surveys                                  | 1.4 million surveys, including eBird |
| **Geographic extent**                                              | Canada only                                          | Canada plus boreal and hemiboreal of US |
| **Temporal resolution**                                            | Predictions for 2017                                 | Predictions at five-year intervals from 1990 to 2020 |
| **Model subregions**                                           | Bird conservation region (BCR)          | Updated BCRs and country |
| **Environmental predictors**                                       | Landcover, biomass, climate                          | Time-matched predictors for vegetation biomass, human disturbance, and annual climate |
| **Model reliability information**                                  | Cross-validated model performance                    | Cross-validated model performance, map of coefficient of variation across bootstraps, map of model extrapolation, map of detection distribution |

## Installation

You can install the most recent version of `BAMexploreR` directly from this repository with:

``` r
# install.packages("remotes")
remotes::install_github("borealbirds/BAMexploreR")
```

## Usage

There are three general categories of tasks that `BAMexploreR` provides:

- **1. Access Models** - download rasters of the model predictions and uncertainty for pre-set regions or custom areas of interest.
- **2. Distribution and Abundance** - explore bird species distribution and estimate population size using the downloaded rasters.
- **3. Habitat Relationships** - explore important predictors of boreal bird abundance and distribution.

You can find vignettes for each category as well as an introductory vignette within the package!

All functions begin with a `bam_*` prefix for ease of use. 

## Issues

To report bugs, request additional features, or get help using the package, please file an [issue](https://github.com/borealbirds/BAMexploreR/issues).

## Contributors

We encourage ongoing contributions and collaborations to improve the package into the future! Please issue a pull request if you'd like to contribute to the package.

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildrtrax/blob/master/LICENSE)©2024 [Alberta Biodiversity Monitoring Institute](https://abmi.ca).

## Citation

To cite `BAMexploreR` package and the BAM density models in publications, please cite the package and the publication:

``` r
bibentry(
  bibtype = "Manual",
  title = "BAMexploreR: model-based density, distribution, and habitat associations of boreal birds",
  author = c(
    person("Mélina", "Houle"),
    person("Mannfred", "Boehm"),
    person("Siu Chung", "Wu"),
    person("Elly C.", "Knight")
    ),
  year = format(Sys.Date(), "%Y"),
  note = paste0("R package version ", packageDescription("BAMexploreR")$Version),
  url = "https://github.com/borealbirds/BAMexploreR"
)
```

``` r 
bibentry(
  bibtype = "Article",
  title = "A generalized modeling framework for spatially extensive species abundance prediction and population estimation",
  author = c(
    person("Diana", "Stralberg"),
    person("Péter", "Sólymos"),
    person("Teegan D.S.", "Docherty"),
    person("Andrew D.", "Crosby"),
    person("Steven L.", "Van Wilgenburg"),
    person("Elly C.", "Knight"),
    person("Anna", "Drake"),
    person("Mannfred M.A.", "Boehm"),
    person("Samuel", "Haché"),
    person("Lionel", "Leston"),
    person("Judith D.", "Toms"),
    person("Jeffrey R.", "Ball"),
    person("Samantha J.", "Song"),
    person("Fiona K.A.", "Schmiegelow"),
    person("Steven G.", "Cumming"),
    person("Erin M.", "Bayne")
  ),
  journal = "Ecosphere",
  year = "In press"
)
```
