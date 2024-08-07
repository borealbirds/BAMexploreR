% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sppList.R
\docType{methods}
\name{sppList}
\alias{sppList}
\title{Retrieve list of species}
\usage{
sppList(version, layer, type)
}
\arguments{
\item{version}{character; Indicate the version of the National Model requested. Each version of the
National Model has its url access provided within the package.}

\item{layer}{character; Name of the output layers of interest, either "mean", "sd" or "overextrapolated".}

\item{type}{character; type of output provided in the list, either "species_code", "common_name", "family_name" or "scientific_name".}
}
\value{
Vector of available url to download
}
\description{
The function produce a character vector of species available to download. To
retrieve available species, the function derive the list using the version, and type of output
}
\examples{
speciesList <- sppList("v4", "mean", "species_code")
}
\author{
Melina Houle
}
