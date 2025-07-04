% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bam_get_layer.R
\docType{methods}
\name{bam_get_layer}
\alias{bam_get_layer}
\title{Download National Model species specific output map raster based on list of species}
\usage{
bam_get_layer(
  spList,
  version,
  destfile,
  crop_ext = NULL,
  year = NULL,
  bcrNM = "mosaic"
)
}
\arguments{
\item{spList}{character. A vector of species to be downloaded.}

\item{version}{character. Represents the version of the national model output to be downloaded.}

\item{destfile}{character. Indicate output path where the downloaded file is saved.}

\item{crop_ext}{SpatVector or SpatRaster used to define the extent for the cropping.
Or downloading valid BCR polygons from list, type: \code{bam_map_bcr("v4")} or \code{bam_map_bcr("v5")}}

\item{year}{character; Specify the year for which the density map were generated. Only in v5.}

\item{bcrNM}{character; vector representing the BCR according to model version. Default is "mosaic".}
}
\value{
A list of `SpatRaster` objects. In addition to returning these objects,
the function also downloads raster files to the directory specified by \code{destfile},
as a side-effect.
}
\description{
Download National Model species specific output map raster based on list of species
}
\examples{
bird <- bam_get_layer("TEWA", "v4", tempfile())

bird <- bam_get_layer("TEWA", "v4", destfile = tempdir())

}
\author{
Melina Houle
}
