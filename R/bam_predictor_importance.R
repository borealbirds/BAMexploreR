##################################################################################
#' Plot Side-by-Side Variation in Predictor Importance by Predictor Class
#'
#' @description Plot mean relative predictor importance (with bootstrap variation)
#' by predictor class, with options to group by BCR (Bird Conservation Region) or species.
#'
#' @param species A \code{character} specifying the species to filter by. The default is \code{"all"}, which includes all species in the dataset.
#'
#' @param bcr A \code{character} specifying the Bird Conservation Regions (BCRs) to filter by. The default is \code{"all"}, which includes all BCRs in the dataset.
#' See \code{bam_map_bcr()} for available BCRs and their spellings.
#'
#' @param group A \code{character} specifying the grouping variable for summarizing predictor importance.
#' Valid strings are \code{"spp"} (species), or \code{"bcr"} (BCR).
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
#'
#' @param viridis_option A \code{character}. Default is \code{"viridis"}. See \code{?ggplot2::scale_colour_viridis_d} for more options.
#'
#' @return A ggplot displaying percent predictor importance by predictor class, grouped by the \code{group} argument.
#' Percent importance is used to allow comparisions across groups that have
#' differing total predictor importance.
#' If \code{plot = FALSE} the processed data is returned as a \code{data.frame},
#' including \code{mean_share} (the mean per-model share of importance for that
#' predictor class), \code{n_units} (the number of models behind it),
#' \code{percent_inf}, its standard error \code{sd_percent_inf}, and
#' \code{sd_among_inf}, the spread of \code{percent_inf} across the models in the
#' group. \code{sd_among_inf} does not shrink as more models are added, so prefer it
#' when comparing groups that differ widely in how many models they contain.
#'
#' @details Relative influence is normalised to sum to 100 \emph{within a single
#' model}, that is, within one species x BCR. Importance is therefore first converted
#' to a share within each model and then averaged over models, so that every model
#' contributes equally and a widely modelled species does not outweigh a narrowly
#' modelled one. A predictor class absent from a model counts as a share of zero.
#'
#' Uncertainty combines two sources: variation among the models in a group
#' (among-BCR, or among-species, variation) and bootstrap variation within each
#' model. \code{sd_percent_inf} is the standard error of the mean share,
#'
#' \deqn{SE = \sqrt{ \frac{s^{2}_{among}}{n} + \frac{\overline{s^{2}_{within}}}{n b} }}
#'
#' for \eqn{n} models and \eqn{b} bootstraps. With a single model this reduces to
#' the bootstrap standard error alone. Being a standard error of a mean, it narrows
#' as a species (or BCR) is modelled more widely; that is correct but means bar
#' widths are not themselves comparable across groups of very different size. The
#' returned \code{sd_among_inf} gives the coverage-neutral spread instead.
#'
#' Because relative influence is compositional, predictors within one model are
#' negatively correlated by construction, so the within-model variance must be
#' computed by summing relative influence within a predictor class \emph{inside
#' each bootstrap}. When bootstrap-level shares are unavailable the function falls
#' back to a root-sum-square approximation that assumes independence and warns; those
#' error bars are conservative (too wide).
#'
#' @importFrom dplyr group_by filter summarise left_join mutate distinct count semi_join coalesce select
#' @importFrom stats var median
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point labs theme theme_classic element_text position_dodge scale_colour_manual
#'
#' @export
#' @examples
#'
#'
#' # Example of plotting predictor importance for Townsend's Solitaire across all BCRs.
#' # This is a species with relatively high bootstrap variance.
#' bam_predictor_importance(species = "TOSO")
#'
#' # Example of plotting predictor importance for two warbler species from three BCRs,
#' # using custom colours:
#' bam_predictor_importance(species = c("BAWW", "CAWA"), group = "spp",
#' bcr = c("can12", "can13", "can14"),  colours = c("#1f78b4", "#33a02c"))
#'
#'
#'
##################################################################################

bam_predictor_importance <- function(species = "all", bcr = "all", group = "spp", version = "v5", plot = TRUE, colours = NULL, viridis_option = "viridis") {

  # validate data version
  if (!version %in% c("v4", "v5")) {
    stop("Invalid `version`. Use 'v4' or 'v5'")
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

  # check if user specified `group` is in `data`. `group` must identify a model,
  # because relative influence is normalised within a species x BCR.
  if (is.null(group) || length(group) != 1 || !group %in% c("spp", "bcr")) {
    stop("`group` must be either 'spp' (species) or 'bcr' (Bird Conservation Region).")
  }


  # filter for user-specified species
  if (!identical(species, "all")) {
    data <- filter(data, spp %in% species)
  }

  # filter for user-specified BCRs
  if (!identical(bcr, "all")) {
    data <- dplyr::filter(data, bcr %in% .env$bcr)
  }

  # convert characters to symbols for dplyr::group_by
  group_sym <- rlang::syms(unique(c(group, "predictor_class")))

  # A species x BCR is one model, and relative influence sums to 100 within it.
  # Work in per-model shares so that every model carries equal weight regardless
  # of how many models a species (or BCR) contributes.
  data <- filter(data, !is.na(predictor_class))

  # `n_boots` counts the bootstraps in which a predictor was non-zero, not the
  # number of replicates run, so estimate the replicate count from the strongest
  # predictor in the whole model rather than from one class.
  model_boot <-
    data |>
    group_by(spp, bcr) |>
    summarise(n_boot = max(n_boots), .groups = "drop")

  # per-model share of each predictor class, plus an approximate bootstrap
  # variance of that share used only when bootstrap-level data is unavailable
  unit_share <-
    data |>
    group_by(spp, bcr, predictor_class) |>
    summarise(share      = sum(mean_rel_inf) / 100,
              approx_var = sum(sd_rel_inf^2) / 100^2,
              .groups    = "drop") |>
    left_join(model_boot, by = c("spp", "bcr"))

  # Prefer bootstrap-level shares: summing relative influence within a class
  # inside each bootstrap respects the negative correlation among predictors of
  # the same model, which sqrt(sum(sd_rel_inf^2)) ignores.
  boot_tbl <- .predictor_class_boot(version)

  if (is.null(boot_tbl)) {
    .warn_once(
      paste0("no_boot_shares_", version),
      "Bootstrap-level predictor-class shares are not available for version '",
      version, "'. Error bars use an approximate within-model variance that ",
      "treats predictors in a class as independent, and so are conservative ",
      "(too wide)."
    )
    unit_share$unit_var <- unit_share$approx_var
  } else {
    unit_var_tbl <-
      boot_tbl |>
      dplyr::semi_join(dplyr::distinct(data, spp, bcr), by = c("spp", "bcr")) |>
      group_by(spp, bcr, predictor_class) |>
      summarise(unit_var = stats::var(share),
                n_boot   = dplyr::n(),
                .groups  = "drop")

    unit_share <-
      unit_share |>
      dplyr::select(-n_boot) |>
      left_join(unit_var_tbl, by = c("spp", "bcr", "predictor_class")) |>
      mutate(unit_var = dplyr::coalesce(unit_var, approx_var))
  }

  # number of models behind each level of `group`. A predictor class absent from
  # a model contributes a share of zero, so divide by this rather than by the
  # number of rows actually present.
  n_units <-
    data |>
    dplyr::distinct(spp, bcr) |>
    dplyr::count(!!rlang::sym(group), name = "n_units")

  # Sums of shares and squared shares let the zero-filled mean and among-model
  # variance be recovered without materialising the absent model x class cells.
  cov_importance_grouped <-
    unit_share |>
    group_by(!!!group_sym) |> # !!! evaluates a list of expressions
    summarise(sum_share    = sum(share),
              sum_share_sq = sum(share^2),
              sum_unit_var = sum(unit_var, na.rm = TRUE),
              n_boot       = stats::median(n_boot, na.rm = TRUE),
              .groups      = "drop") |>
    left_join(n_units, by = group) |>
    mutate(
      mean_share = sum_share / n_units,
      # among-model variance of the per-model share (zero-filled)
      among_var  = ifelse(n_units > 1,
                          (sum_share_sq - n_units * mean_share^2) / (n_units - 1),
                          0),
      # mean within-model (bootstrap) variance, also zero-filled
      within_var = sum_unit_var / n_units,
      # standard error of the mean share, propagating both components
      se_share   = sqrt(pmax(among_var, 0) / n_units +
                        within_var / (n_units * n_boot)),
      # spread of the per-model share across models. Unlike `se_share` this does
      # not shrink as more models are added, so it is the better summary when
      # comparing groups that differ widely in how many models they contain.
      sd_among   = sqrt(pmax(among_var, 0))
    )

  # Shares sum to 1 within every model, so the class shares of a group sum to 1
  # and `sum_all_groups` is 1 up to floating point. It is kept explicit so the
  # percentages are exact and so the code still behaves if that ever changes.
  group1_sum <-
    cov_importance_grouped |>
    group_by(!!group_sym[[1]]) |>
    summarise(sum_all_groups = sum(mean_share), .groups = "keep")

  # calculate the percent of predictor importance
  # sd_percent_inf is the uncertainty of the percent influence of a given predictor_class
  percent_importance <-
    cov_importance_grouped |>
    left_join(group1_sum, by = group) |>
    mutate(percent_inf = 100 * mean_share / sum_all_groups,
           sd_percent_inf = 100 * se_share / sum_all_groups,
           sd_among_inf   = 100 * sd_among / sum_all_groups) |>
    # drop the running sums and variance components used to build the estimates
    dplyr::select(-sum_share, -sum_share_sq, -sum_unit_var, -n_boot,
                  -among_var, -within_var, -se_share, -sd_among)

  percent_importance[[group]] <- as.factor(percent_importance[[group]])

  if (plot) {

    p <- ggplot2::ggplot(
      percent_importance,
      ggplot2::aes(
        x = predictor_class,
        y = percent_inf,
        fill = !!rlang::sym(group),
        colour = !!rlang::sym(group)
      )
    ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.75),
        alpha = 0.7,
        size = 2.5
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymax = percent_inf + sd_percent_inf,
          ymin = percent_inf - sd_percent_inf
        ),
        position = ggplot2::position_dodge(width = 0.75),
        width = 0,
        linewidth = 0.75
      ) +
      ggplot2::labs(
        x = "Predictor Class",
        y = "Relative Importance (%)",
        title = paste("Predictor importance by", group)
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    if (is.null(colours)) {

      # colour-blind friendly default
      p <- p +
        ggplot2::scale_colour_viridis_d(option = viridis_option) +
        ggplot2::scale_fill_viridis_d(option = viridis_option)

    } else {

      n_levels <- length(unique(percent_importance[[group]]))

      if (length(colours) != n_levels) {
        stop(paste0(
          "Length of `colours` (", length(colours),
          ") must match number of group levels (", n_levels, ")."
        ))
      }

      p <- p +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::scale_fill_manual(values = colours)
    }

    return(p)

  } else {

    return(percent_importance)

  }
}
