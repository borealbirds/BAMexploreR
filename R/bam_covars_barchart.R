##################################################################################
#' Visualize Covariate Importance as Stacked Bar Charts
#'
#' Creates stacked bar plots showing the proportion of model covariates importance
#' in predicting bird abundance.
#'
#'
#'
#' @param species A \code{character} specifying the species to filter by. The default is \code{"all"}, which includes all species in the dataset.
#'
#' @param bcr A \code{character} specifying the model subregions, or Bird Conservation Regions (BCRs) to filter by. The default is \code{"all"}, which includes all BCRs in the dataset.
#'
#' @param groups A \code{character} of two grouping variables for summarising covariate importance.
#' The first group element is plotted on the x-axis as bins each containing a stacked bar,
#' while the second group element is shown by fill colours in the stacked bars.
#' Valid strings are any two of: \code{"spp"} (species), \code{"bcr"} (BCR; model subregion), \code{"var"} (model variable), or \code{"var_class"} (model variable category).
#' Please see the examples below for a visualization.
#'
#' @param version A \code{character}. Defaults to \code{"v5"}. Loads BAM's covariate importance data,
#' a \code{data.frame} containing covariate importance values, with mean covariate importance as
#' rows and columns \code{bcr}, \code{species}, \code{var_clas}, \code{n_boot}, \code{mean_rel_inf}, and \code{sd_rel_inf}.
#' \code{"v4"} is also possible but not fully supported for all functions in the first release of this package.
#'
#' @param plot A \code{logical} indicating whether to plot the results (\code{TRUE}) or return the processed data (\code{FALSE}).
#'
#' @param colours A \code{character} vector of hex codes for the colours to use in the ggplot (optional).
#' If \code{NULL}, default colours are used.
#'
#' @return A stacked bar chart.
#'
#' @details Stacked bars can be grouped by species, variable class, or
#' Bird Conservation Region (BCR). For example, grouping by species and variable class creates
#' a plot where a stacked bar is created for each species, and each bar is split into the proportion
#' of covariate importance that each of nine variable classes contributed, pooled across the specified BCRs.
#'
#'
#' @importFrom rlang syms
#' @importFrom dplyr filter summarise group_by left_join mutate
#' @importFrom ggplot2 ggplot aes geom_bar theme theme_classic element_text scale_fill_manual labs
#'
#' @export
#'
#' @examples

#' # Compare covariate importance (binned by variable class) for all species in all BCRs
#' bam_covars_barchart(species = "all", bcr = "all",  groups = c("spp", "var_class"))
#'
#' # Compare covariate importance (binned by variable class) in the Prairies (BCRs 11, 6-1, 6-0)
#' # to the Pacific Coast across (BCR 5) all species
#' prairies_to_coast <- c("can11", "can60", "can61", "can5")
#' bam_covars_barchart(species = "all", bcr = prairies_to_coast, groups=c("bcr", "var_class"))
#'
#' # Compare covariate importance (binned by variable class) for four
#' # warbler species in BCR14
#' warblers <- c("CAWA", "BAWW", "BTNW", "BLBW")
#' bam_covars_barchart(species = warblers, bcr = "can14", groups = c("spp", "var_class"))
#'
#' # Compare covariate importance for a single warbler species
#' # relative to the total influence that covariate had across all warblers.
#' bam_covars_barchart(species = warblers, bcr = "can14", groups = c("var", "spp"))


bam_covars_barchart <- function(species = "all", bcr = "all",  groups = c("spp", "var_class"), version ="v5", plot = TRUE, colours = NULL){

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }

  # load bam_covariate_importance_v* from data folder
  #load(system.file("R/sysdata.rda", package = "BAMexploreR"))
  if (version == "v5") {
    #data("bam_covariate_importance_v5", package = "BAMexploreR")
    data <- bam_covariate_importance_v5
  } else {
    #data("bam_covariate_importance_v4", package = "BAMexploreR")
    data <- bam_covariate_importance_v4
  }

  # check if user specified species are in `data`
  if (!all(species %in% unique(data$spp)) && !any(species == "all")) {
    stop(paste("The following species are not in `data`:",
               paste(setdiff(species, unique(data$spp)), collapse = ", ")))
  }

  # check if user specified BCRs are in `data`
  if (!all(bcr %in% unique(data$bcr)) && !any(bcr == "all")) {
    stop(paste("The following BCR(s) are not in `data`:",
               paste(setdiff(bcr, unique(data$bcr)), collapse = ", ")))
  }

  # filter for user-specified species
  if (!any(species == "all")) {
    data <- filter(data, spp %in% species)
  }

  # filter for user-specified BCRs
  # use .env because `bcr` is also a column name in `data`
  if (!any(bcr == "all")) {
    data <- dplyr::filter(data, bcr %in% .env$bcr)
  }

  # ensure groups are specified correctly
  if (is.null(groups) || length(groups) < 2) {
    stop("The 'groups' parameter must be a character vector with at least two elements.")
  }

  # check groups exist in data
  if (!all(groups %in% colnames(data))) {
    stop("One or more elements in `groups` are not valid column names in `data`.")
  }


  # for dplyr::group_by
  group_syms <- rlang::syms(groups)


  # sum covariate importance across for every permutation of group1 and group2
  rel_inf_sum <-
    data |>
    drop_na() |>
    group_by(!!!group_syms) |>
    summarise(sum_influence = sum(mean_rel_inf), .groups="keep")


  # sum of covariate importance for each of group1 (all group2 sums are amalgamated into group1 bins)
  group1_sum <-
    rel_inf_sum |>
    group_by(!!group_syms[[1]])  |>
    summarise(sum_group1 = sum(sum_influence), .groups="keep")

  # get the %contribution of group2 covariates to overall covariate importance for a given group1
  proportion_inf <-
    rel_inf_sum |>
    left_join(x = _, group1_sum, by=groups[1]) |>
    mutate(prop = sum_influence/sum_group1)


  if (plot) {
    p <- ggplot2::ggplot(proportion_inf, ggplot2::aes(x = !!group_syms[[1]], y = prop, fill = !!group_syms[[2]])) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(x = groups[1], y = "Proportion of Influence", fill = groups[2])

    if (!is.null(colours)) {
      p <- p + ggplot2::scale_fill_manual(values = colours)
    }

    print(p)
    return(p)
  } else {
    return(proportion_inf)
  }
}


