##################################################################################
#' Visualize Predictor Importance as Stacked Bar Charts
#'
#' Creates stacked bar plots showing the proportion of model predictors importance
#' in predicting bird abundance.
#'
#'
#'
#' @param species A \code{character} specifying the species to filter by. The default is \code{"all"}, which includes all species in the dataset.
#' See \code{data(spp_tbl)} for available species and their spelling.
#'
#' @param bcr A \code{character} specifying the model subregions, or Bird Conservation Regions (BCRs) to filter by.
#' The default is \code{"all"}, which includes all BCRs in the dataset.
#' See \code{bam_map_bcr()} for available BCRs and their spellings.
#'
#' @param groups A \code{character} of two grouping variables for summarising predictor importance.
#' The first group element is plotted on the x-axis as bins each containing a stacked bar,
#' while the second group element is shown by fill colours in the stacked bars.
#' Valid strings are any two of: \code{"spp"} (species), \code{"bcr"} (BCR; model subregion), \code{"predictor"} (model predictor), or \code{"predictor_class"} (model predictor class).
#' Please see the examples below for a visualization.
#'
#' @param version Specifies the model release: \code{"v5"} for the current
#'   models or \code{"v4"} for the archived models. Defaults to \code{"v5"}.
#'   Loads the corresponding predictor-importance \code{data.frame}, with columns
#'   \code{bcr}, \code{species}, \code{predictor_class}, \code{n_boot},
#'   \code{mean_rel_inf}, and \code{sd_rel_inf}.
#'
#' @param plot A \code{logical} indicating whether to plot the results (\code{TRUE}) or return the processed data (\code{FALSE}).
#'
#' @param colours A \code{character} vector of hex codes or colour names (optional).
#' If \code{NULL}, default colour-blind friendly palette from \code{viridis} is used.
#'
#' @param viridis_option A \code{character}. Default is \code{"viridis"}. See \code{?ggplot2::scale_colour_viridis_d} for more options.
#'
#' @return A stacked bar chart with the first group element plotted on the x-axis as bins each containing a stacked bar, and the second group element is shown by fill colours in the stacked bars.  If plot = FALSE the processed data is returned as a data.frame.
#' The returned \code{data.frame} includes \code{mean_share}, the mean per-model share of
#' predictor importance (see Details), and \code{prop}, that share expressed as a proportion
#' of its x-axis bin. Rank predictors on \code{mean_share} or \code{prop} rather than on a
#' raw sum of importance.
#'
#' @details Stacked bars can be grouped by species, predictor class, or
#' Bird Conservation Region (BCR). For example, grouping by species and predictor class creates
#' a plot where a stacked bar is created for each species, and each bar is split into the proportion
#' of predictor importance that each of nine predictor classes contributed, pooled across the specified BCRs.
#'
#' Relative influence is normalised by the boosted regression tree machinery to sum to 100
#' \emph{within a single model}, that is, within one species x BCR. Importance values therefore
#' only have meaning relative to other predictors in the same species x BCR, and a raw sum across
#' models would weight a species (or a BCR) by how many models it appears in. Species in this
#' dataset are modelled in anywhere from 4 to 33 BCRs, so that weighting is substantial.
#'
#' To keep comparisons valid, importance is first converted to a share within each species x BCR
#' model, then averaged over models so that every model contributes equally, and only then
#' expressed as a proportion of its x-axis bin. Bars are therefore comparable across species and
#' BCRs regardless of how widely each was modelled. Note that a species' profile is still an
#' average over the particular BCRs in which it was modelled; to compare species on identical
#' geography, restrict \code{bcr} to a set they share.
#'
#' \code{groups} must pair one model-level variable (\code{"spp"} or \code{"bcr"}) with one
#' within-model variable (\code{"predictor"} or \code{"predictor_class"}). Pairing two of the
#' same kind is degenerate and raises an error.
#'
#'
#' @importFrom rlang syms
#' @importFrom dplyr filter summarise group_by left_join mutate distinct count
#' @importFrom ggplot2 ggplot aes geom_bar theme theme_classic element_text scale_fill_manual labs
#'
#' @export
#'
#' @examples

#' # Compare predictor importance (binned by predictor class) for all species in all BCRs
#' bam_predictor_barchart(species = "all", bcr = "all",  groups = c("spp", "predictor_class"))
#'
#' # Compare predictor importance (binned by predictor class) in the Prairies (BCRs 11, 6-1, 6-0)
#' # to the Pacific Coast across (BCR 5) all species
#' prairies_to_coast <- c("can11", "can60", "can61", "can5")
#' bam_predictor_barchart(species = "all", bcr = prairies_to_coast, groups=c("bcr", "predictor_class"))
#'
#' # Compare predictor importance (binned by predictor class) for four
#' # warbler species in BCR14
#' warblers <- c("CAWA", "BAWW", "BTNW", "BLBW")
#' bam_predictor_barchart(species = warblers, bcr = "can14", groups = c("spp", "predictor_class"))
#'
#' # Compare predictor importance for a single warbler species
#' # relative to the total influence that predictor had across all warblers.
#' bam_predictor_barchart(species = warblers, bcr = "can14", groups = c("predictor", "spp"))


bam_predictor_barchart <- function(species = "all", bcr = "all",  groups = c("spp", "predictor_class"), version ="v5", plot = TRUE, colours = NULL, viridis_option = "viridis"){

  if (!version %in% c("v4", "v5")) {
    stop("Invalid version argument. Must be either 'v4' or 'v5'.")
  }

  # load bam_predictor_importance_v* from data folder
  # `.load_predictor_importance()` rescales v4 (which is truncated at rel.inf >= 1)
  # so that every species x BCR sums to 100, as v5 already does.
  data <- .load_predictor_importance(version)

  # convert user specified species to FLBCs
  if (!identical(species, "all")){
    species <- standardize_species_names(
      species_input = species,
      spp_tbl = BAMexploreR:::spp_tbl,
      version = version
    )
  }

  # check if user specified species are in `data`
  if (!all(species %in% unique(data$spp)) && !identical(species, "all")) {
    stop(paste("The following species are not in `data`:",
               paste(setdiff(species, unique(data$spp)), collapse = ", ")))
  }

  # check if user specified BCRs are in `data`
  if (!all(bcr %in% unique(data$bcr)) && !identical(bcr, "all")) {
    stop(paste("The following BCR(s) are not in `data`:",
               paste(setdiff(bcr, unique(data$bcr)), collapse = ", ")))
  }

  # filter for user-specified species
  if (!identical(species, "all")) {
    data <- filter(data, spp %in% species)
  }

  # filter for user-specified BCRs
  # use .env because `bcr` is also a column name in `data`
  if (!identical(bcr, "all")) {
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

  # `spp` and `bcr` identify a model; `predictor` and `predictor_class` partition
  # the predictors within a model. Pairing two variables of the same kind is
  # degenerate, so reject it rather than drawing a misleading plot.
  unit_vars   <- intersect(groups, c("spp", "bcr"))
  within_vars <- setdiff(groups, unit_vars)

  if (length(unit_vars) == 2) {
    stop("`groups` cannot be both 'spp' and 'bcr'. Relative influence is normalised ",
         "within each species x BCR model, so every stacked segment would be exactly ",
         "1/n. Pair one of 'spp'/'bcr' with 'predictor' or 'predictor_class'.")
  }

  if (length(within_vars) == 2) {
    stop("`groups` cannot be both 'predictor' and 'predictor_class'. Each predictor ",
         "belongs to exactly one class, so every bar would carry a single fill. ",
         "Pair one of them with 'spp' or 'bcr'.")
  }

  # for dplyr::group_by
  group_syms <- rlang::syms(groups)

  # Relative influence sums to 100 within each species x BCR model. Aggregating raw
  # sums would therefore weight a species (or BCR) by how many models it appears in,
  # so a widely modelled species would dominate any bar it is stacked into. Instead
  # convert to a per-model share first, then average over models so that each model
  # contributes equally, and only then normalise within the x-axis group.

  # drop predictors with no assigned class before any normalisation, so that
  # shares and model counts are derived from exactly the same rows
  data <- filter(data, !is.na(predictor_class))

  # 1. share of each cell within a single model (each model contributes total mass 1)
  unit_share <-
    data |>
    group_by(spp, bcr, !!!rlang::syms(within_vars)) |>
    summarise(share = sum(mean_rel_inf) / 100, .groups = "drop")

  # 2. number of models contributing to each stratum, giving every model equal weight
  n_units <-
    data |>
    dplyr::distinct(spp, bcr) |>
    dplyr::count(!!!rlang::syms(unit_vars), name = "n_units")

  # 3. mean per-model share for every permutation of group1 and group2
  rel_inf_sum <-
    unit_share |>
    group_by(!!!group_syms) |>
    summarise(sum_share = sum(share), .groups = "drop") |>
    left_join(x = _, n_units, by = unit_vars) |>
    mutate(mean_share = sum_share / n_units)

  # mean share for each of group1 (all group2 shares are amalgamated into group1 bins)
  group1_sum <-
    rel_inf_sum |>
    group_by(!!group_syms[[1]])  |>
    summarise(sum_group1 = sum(mean_share), .groups="keep")

  # get the %contribution of group2 predictors to overall predictor importance for a given group1
  proportion_inf <-
    rel_inf_sum |>
    left_join(x = _, group1_sum, by=groups[1]) |>
    mutate(prop = mean_share/sum_group1)

  proportion_inf[[groups[2]]] <- as.factor(proportion_inf[[groups[2]]])

  if (plot) {
    p <- ggplot2::ggplot(proportion_inf, ggplot2::aes(x = !!group_syms[[1]], y = prop, fill = !!group_syms[[2]])) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(x = groups[1], y = "Proportion of Influence", fill = groups[2])

    if (is.null(colours)) {
      p <- p + ggplot2::scale_fill_viridis_d(option = viridis_option)

    } else {

      # determine number of fill levels
      n_levels <- length(unique(proportion_inf[[groups[2]]]))

      if (length(colours) != n_levels) {
        stop(paste0(
             "Length of `colours` (", length(colours),
             ") must match number of fill levels (", n_levels, ").")
            )
      }

      p <- p +
        ggplot2::scale_fill_manual(values = colours)
    }

    return(p)

  } else {

    return(proportion_inf)
  }

} # close function




