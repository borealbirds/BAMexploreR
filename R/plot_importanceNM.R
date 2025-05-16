##################################################################################
#' Plot Side-by-Side Variation in Covariate Importance by Variable Class
#'
#' This function plots mean relative covariate importance (with bootstrap variation)
#' by variable class, with options to group by BCR (Bird Conservation Region) or species.
#'
#'
#' @param species A `character` specifying the species to filter by. The default is `"all"`, which includes all species in the dataset.
#'
#' @param bcr A `character` specifying the Bird Conservation Regions (BCRs) to filter by. The default is `"all"`, which includes all BCRs in the dataset.
#'
#' @param group A `character` specifying the grouping variable for summarizing covariate importance.
#' Valid strings are `"spp"` (species), or `"bcr"` (BCR).
#'
#' @param version A `character`. Defaults to `"v5"`. Loads BAM's covariate importance data,
#' a `data.frame` containing covariate importance values, with mean covariate importance as
#' rows and columns `bcr`, `species`, `var_class`, `n_boot`, `mean_rel_inf`, and `sd_rel_inf`.
#' `"v4"` is also possible but not fully supported for all functions in the first release of this package.
#'
#' @param plot A `logical` indicating whether to plot the results (`TRUE`) or return the processed data (`FALSE`).
#'
#' @param colours A `character` vector of hex codes for the colours to use in the ggplot (optional).
#' If `NULL`, default colours are used.
#'
#' @return A ggplot displaying percent covariate importance by variable class, grouped by the `group` argument.
#' Percent importance is used to allow comparisions across groups that have
#' differing total covariate importance.
#' If `plot = FALSE` the processed data is returned as a `data.frame`.
#'
#' @importFrom dplyr group_by filter summarise left_join mutate
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point labs theme theme_classic element_text
#' @importFrom tidyr drop_na
#'
#' @export
#' @examples
#'
#'
#' # Example of plotting covariate importance for Townsend's Solitaire across all BCRs.
#' # This is a species with relatively high bootstrap variance.
#' plot_importanceNM(group = "spp", species = "TOSO", bcr = "all", plot=TRUE)
#'
#' # Example of plotting covariate importance for two warbler species from three BCRs, using custom colours:
#'  plot_importanceNM(group = "spp", species = c("BAWW", "CAWA"),
#'  bcr = c("can12", "can13", "can14"),  colours = c("#1f78b4", "#33a02c"))
#'
#'
#'
##################################################################################

plot_importanceNM <- function(species = "all", bcr = "all", group = NULL, version = "v5", plot = TRUE, colours = NULL) {

  # validate data version
  if (!version %in% c("v4", "v5")) {
    stop("Invalid `version`. Use 'v4' or 'v5'")
  }

  # load bam_covariate_importance_v* from data folder
  if (version == "v5") {
    data("bam_covariate_importance_v5", package = "BAMexploreR")
    data <- bam_covariate_importance_v5
  } else {
    data("bam_covariate_importance_v4", package = "BAMexploreR")
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

  # check if user specified `group` is in `data`
  if (is.null(group) || !group %in% colnames(data)) {
    stop("Please specify a valid `group` column from the data.")
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

  # check if user specified `colours` match the number of levels in `group`.
  if (!is.null(colours)) {
    n_groups <- length(unique(data[[group]]))
    if (length(colours) != n_groups) {
      stop(paste("The length of `colours` does not match the number of levels in `group`", n_groups))
    }
  }


  # convert characters to symbols for dplyr::group_by
  group_sym <- rlang::syms(unique(c(group, "var_class")))


  # group by user-specified group
  # then, sum rel.inf by the grouped variable per variable class
  # convert std. dev. back to variance, sum, and take sqrt()
  cov_importance_grouped <-
    data |>
    group_by(!!!group_sym) |> # !!! evaluates a list of expressions
    filter(!is.na(var_class)) |>
    summarise(sum_inf = sum(mean_rel_inf), sd_inf = sd(mean_rel_inf),
              pooled_sd = sqrt(sum(sd_rel_inf^2)),
              .groups = "keep")

  # group by user-specified group,
  # then, sum rel.inf from all var_classes
  group1_sum <-
    cov_importance_grouped |>
    group_by(!!group_sym[[1]]) |>
    summarise(sum_all_groups = sum(sum_inf), .groups = "keep")

  # calculate the percent of covariate importance
  # sd_percent_inf is the uncertainty of the percent influence of a given var_class
  percent_importance <-
    cov_importance_grouped |>
    left_join(group1_sum, by = group) |>
    mutate(percent_inf = 100 * sum_inf / sum_all_groups,
           sd_percent_inf = 100 * pooled_sd / sum_all_groups)


  if (plot) {
    p <- ggplot(percent_importance, aes(x = var_class, y = percent_inf,
                                        fill = !!sym(group), colour = !!sym(group))) +
      geom_point(position = position_dodge(width = 0.75), alpha = 0.7, size = 2.5) +
      geom_errorbar(aes(ymax = percent_inf + sd_percent_inf, ymin = percent_inf - sd_percent_inf),
                    position = position_dodge(width = 0.75), width = 0, linewidth = 0.75) +
      labs(x = "Variable Class", y = "Relative Importance (%)",
           title = paste("Covariate importance by", group)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Apply custom colours only if provided
    if (!is.null(colours)) {
      group_levels <- unique(data[[group]])
      names(colours) <- group_levels
      p <- p + scale_colour_manual(values = colours) +
        scale_fill_manual(values = colours)
    }

    return(p)

  } else {

    return(percent_importance)

  }
}









