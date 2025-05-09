##################################################################################
#' Side-by-Side Boxplots Displaying Bootstrap Variation by Variable Class
#'
#' This function generates side-by-side boxplots that show bootstrap variation of
#' covariate importance by variable class, with options to group by BCR (Bird Conservation Region),
#' species, and other factors.
#'
#' @param data Defaults to BAM's covariate importance data but can be replaced with any `data.frame` containing covariate importance values,
#' with covariates as rows and a column each for `bcr`, `species`, `var_class`, `n_boot`, and `mean_rel_inf`. If user data has no bootstrap replicates,
#' create a `boot` column and fill in the same number (e.g. `1`) for all rows. Leave this argument as `NULL` unless using custom data.
#'
#' @param version A `character`. Defaults to `"v5"`. `"v4"` is also possible but not fully supported for all functions in the first release of this package.
#' If using custom data in the `data` argument, this argument will be ignored.
#'
#' @param species A `character` specifying the species to filter by. The default is `"all"`, which includes all species in the dataset.
#'
#' @param bcr A `character` specifying the Bird Conservation Regions (BCRs) to filter by. The default is `"all"`, which includes all BCRs in the dataset.
#'
#' @param group A `character` specifying the grouping variable(s) for summarizing the covariate importance (e.g., species, BCR, etc.).
#'
#' @param plot A `logical` indicating whether to plot the results (`TRUE`) or return the processed data (`FALSE`).
#'
#' @param colours A `character` vector of hex codes for the colours to use in the boxplot (optional).
#' If `NULL`, default colours are used.
#'
#' @return A ggplot object displaying boxplots of relative covariate importance by variable class, grouped by the `group` argument.
#' If `plot = FALSE`, the processed data is returned instead of the plot.
#'
#' @importFrom dplyr group_by filter summarise left_join mutate
#' @importFrom rlang syms
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point labs theme theme_classic element_text
#' @importFrom tidyr drop_na
#'
#' @export
#' @rdname boxplotNM
#' @examples
#'
#'
#' # Example of plotting covariate importance for Alder Flycatcher across all BCRs.
#' # boxplotNM(group = "species", species = "ALFL", bcr = "all")
#'
#' # Example of plotting covariate importance for two species in two BCRs, using custom colours:
#' # boxplotNM(group = "species", species = c("ALFL", "CAWA"),
#' bcr = c("BCR12", "BCR13"),  colours = c("#1f78b4", "#33a02c"))
#'
#'
#'
##################################################################################

boxplotNM <- function(version = "v5", species = "all", bcr = "all", group = NULL, plot = FALSE, colours = NULL) {

  # validate data version
  if (!version %in% c("v4", "v5") && is.null(data)) {
    stop("Invalid `version`. Use 'v4' or 'v5', or provide your own data frame")
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
  if (!all(species %in% unique(data$spp)) && species != "all") {
    stop(paste("The following species are not in `data`:",
               paste(setdiff(species, unique(data$spp)), collapse = ", ")))
  }

  # check if user specified BCRs are in `data`
  if (!all(bcr %in% unique(data$bcr)) && bcr != "all") {
    stop(paste("The following BCR(s) are not in `data`:",
               paste(setdiff(bcr, unique(data$bcr)), collapse = ", ")))
  }

  # check if user specified `group` is in `data`
  if (is.null(group) || !group %in% colnames(data)) {
    stop("Please specify a valid `group` column from the data.")
  }

  # check if user specified `colours` match the number of levels in `group`.
  if (!is.null(colours)) {
    n_groups <- length(unique(data[[group]]))
    if (length(colours) != n_groups) {
      stop(paste("The length of `colours` does not match the number of levels in `group`
                 (", n_groups, "). Provide a colour for each level."))
    }
  }

  # define BCRs based on user inputs
  ifelse(bcr == "all", bcr_to_filter <- unique(data$bcr), bcr_to_filter <- bcr)

  # for dplyr::group_by
  group_sym <- rlang::syms(unique(c(group, "n_boots")))

  # specify which data variable to group by
  # sum rel. influence of the grouped variable (e.g. species) per `var_class` and `boot` replicate
  rel_inf_sum <-
    data |>
    group_by(!!!group_sym) |>
    filter(bcr %in% bcr_to_filter) |>
    tidyr::drop_na(mean_rel_inf, !!!group_sym) |>
    summarise(sum_influence = sum(mean_rel_inf), .groups="keep")


  # sum of covariate importance for each of group1 (all var_class sums are amalgamated into group1 bins)
  group1_sum <-
    rel_inf_sum |>
    group_by(!!group_sym[[1]], !!group_sym[[2]])  |>
    summarise(sum_group1 = sum(sum_influence), .groups="keep")


  proportion_inf <-
    rel_inf_sum |>
    left_join(x = _, group1_sum, by=group) |>
    mutate(prop = sum_influence/sum_group1)


  if (plot) {

    ggplot(proportion_inf, aes(x = var_class, y = prop, fill = !!group_sym[[1]])) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.05) +
      geom_point(aes(colour = factor(!!group_sym[[1]])),
                 position = position_dodge(width = 0.75), alpha = 0.7, size = 2.5) +
      labs(x = "Variable Class", y = "Relative Importance (%)",
           title = paste("Covariate importance by", group, sep = " ")) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  } else {

    return(proportion_inf)

  }
}









