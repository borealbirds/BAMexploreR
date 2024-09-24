BAMexploreR
================
Melina Houle
2024-09-24

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

`BAMexploreR` provides a set of functions for manipulating version 4 and
5 of the BAM’s landbird models.

You can visit the package website here:
<https://github.com/borealbirds/BAMexploreR>.

## Installation

You can install the development version of `BAMexploreR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("borealbirds/BAMexploreR", force = TRUE, dependencies = TRUE)
devtools::document()
devtools::install()
```

## Citation

Please cite the *BAMexploreR* package when using it in publications. To
cite the latest official version, please use:

\> Houle M, Boehm M, Wu SC, Knight E, … (2024). BAMexploreR: Evaluating
boreal birds density in Canada using R. R package version 0.1.0.
Available at <https://github.com/borealbirds/BAMexploreR>

## Quick Start

This quick start guides shows how to identify species available per
version, identify subregion(s) that intersect a area of interest (AOI)
and download species specific density maps at the right scale.

The package includes demo dataset `vignette.shp` and `vignette.rast`
that represent simulated AOI to demonstrate the workflow of the
different functions.

``` r
library(BAMexploreR)
library(googledrive)
library(tmap)
devtools::load_all()
```

Function : sppList

The function produce a character vector of species available to
download. To retrieve available species, the function derive the list
using the version, and type of output. The function allow to have the
output either by “species_code”, “common_name”, “family_name” or
“scientific_name”.

``` r
# Check which species are part of the National Model v4
#drive_auth()
#ttt <- drive_auth()
#saveRDS(ttt, "token.rds")
if (!googledrive::drive_has_token()) {
    googledrive::drive_auth(cache= TRUE)
}
spp <- sppList("v4", "mean", "species_code")
print(spp)
#>  [1] "BAOR" "BANS" "ATTW" "ATSP" "AMRO" "AMRE" "AMPI" "AMGO" "AMCR" "ALFL"

spp <- sppList("v5", "mean", "species_code")
print(spp)
#> [1] "CAWA" "AMPI" "AMGO"
```

Function : mapBCR

Retrieve list of BCR overlaid by the AOI. If no AOI is provided, the
user can either get a list of everything that is available or look at
the map of how subunit are divided.

``` r
# NO AOI provided
tmap_mode("plot")
subUnitsv4_results <- mapBCR("v4") 
print(subUnitsv4_results$map)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
print(subUnitsv4_results$subUnits)
#>  [1] "can4"  "can5"  "can9"  "can10" "can11" "can12" "can13" "can14" "can60"
#> [10] "can61" "can70" "can71" "can80" "can81" "can82" "can83"

subUnitsv5_results <- mapBCR("v5") 
print(subUnitsv5_results$map)
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

``` r
print(subUnitsv5_results$subUnits)
#>  [1] "can3"      "can5"      "can9"      "can10"     "can11"     "can12"    
#>  [7] "can13"     "can14"     "can40"     "can60"     "can61"     "can70"    
#> [13] "can71"     "can72"     "can80"     "can81"     "can82"     "usa2"     
#> [19] "usa5"      "usa9"      "usa10"     "usa11"     "usa12"     "usa13"    
#> [25] "usa14"     "usa23"     "usa28"     "usa30"     "usa40"     "usa43"    
#> [31] "can4142"   "usa414232"

# with AOI provided
aoi_shp <- system.file("extdata", "vignette_poly_LAEA.shp", package = "BAMexploreR")
aoi_sf <- vect(aoi_shp)
subUnits_results <- mapBCR("v5", ext=aoi_sf) 
print(subUnits_results$map)
```

<img src="man/figures/README-unnamed-chunk-6-3.png" width="100%" />

``` r
print(subUnits_results$subUnits)
#> [1] "can9"  "can10" "usa9"  "usa10"
```
